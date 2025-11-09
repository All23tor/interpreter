#include "Interpreter.hpp"
#include <algorithm>
#include <array>
#include <charconv>
#include <format>
#include <functional>
#include <stdexcept>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>

namespace {
struct Assign {
  template <class T>
  Value operator()(Ref ref, const T& value) {
    return ref.ref->second = {value};
  }
};

template <std::size_t Sz>
struct CompStr {
  char str[Sz];
  constexpr CompStr(const char (&_str)[Sz]) {
    std::copy_n(_str, Sz, str);
  }
};
template <class _T, CompStr _N>
struct NameType {
  using T = _T;
  static constexpr auto N = _N.str;
};
using name_types = std::tuple<
  NameType<int, "int">,
  NameType<float, "float">,
  NameType<bool, "bool">,
  NameType<std::string, "string">,
  NameType<std::monostate, "unit">,
  NameType<Ref, "ref">,
  NameType<std::logical_or<>, "||">,
  NameType<std::logical_and<>, "&&">,
  NameType<std::greater_equal<>, ">=">,
  NameType<std::less_equal<>, "<=">,
  NameType<std::greater<>, ">">,
  NameType<std::less<>, "<">,
  NameType<std::equal_to<>, "==">,
  NameType<std::not_equal_to<>, "!=">,
  NameType<std::plus<>, "+">,
  NameType<std::minus<>, "-">,
  NameType<std::multiplies<>, "*">,
  NameType<std::divides<>, "/">,
  NameType<std::modulus<>, "%">,
  NameType<Assign, "<-">>;
template <class T, std::size_t I = 0>
consteval std::string_view name_type() {
  using Nt = std::tuple_element_t<I, name_types>;
  if constexpr (std::is_same_v<T, typename Nt::T>)
    return Nt::N;
  else
    return name_type<T, I + 1>();
}

namespace sv {
// necessary to avoid ugly static_cast<unsigned char> everywhere
constexpr auto is_alnum = [](unsigned char c) { return std::isalnum(c); };
constexpr auto is_digit = [](unsigned char c) { return std::isdigit(c); };
constexpr auto is_space = [](unsigned char c) { return std::isspace(c); };
std::string_view ltrim(std::string_view sv) {
  auto it = std::ranges::find_if_not(sv, is_space);
  return sv.substr(it - sv.begin());
}
std::string_view rtrim(std::string_view sv) {
  auto it = std::ranges::find_last_if_not(sv, is_space).begin();
  return sv.substr(0, (it - sv.begin()) + 1);
}
std::string_view trim(std::string_view sv) {
  return rtrim(ltrim(sv));
}
bool is_variable_name(std::string_view name) {
  return !name.empty() && !sv::is_digit(name.front()) &&
    std::ranges::all_of(name, [](unsigned char c) {
      return sv::is_alnum(c) || c == '_';
    });
}
} // namespace sv

using NodePtr = std::unique_ptr<Node>;
struct LiteralNode final : public Node {
  const Value value;

  LiteralNode(Value _value) : value{std::move(_value)} {}
  virtual ~LiteralNode() override = default;
  virtual Value evaluate(Context&) const override {
    return value;
  }
};

struct VariableNode final : public Node {
  const std::string name;

  VariableNode(std::string_view _name) :
    name([](auto _name) {
      if (!sv::is_variable_name(_name))
        throw std::invalid_argument(
          std::format("'{}' is not a valid variable name", _name)
        );
      return _name;
    }(_name)) {}
  virtual ~VariableNode() override = default;
  virtual Value evaluate(Context& ctx) const override {
    try {
      return ctx.at(name);
    } catch (const std::out_of_range&) {
      throw std::runtime_error(std::format("{} was not declared", name));
    }
  }
};

template <class F>
struct BinaryOperationNode final : public Node {
  static constexpr std::string_view op_name = name_type<F>();
  NodePtr left;
  NodePtr right;

  BinaryOperationNode(std::string_view expr, std::size_t pos) :
    left(parse_expression(expr.substr(0, pos))),
    right(parse_expression(expr.substr(pos + op_name.size()))) {}
  virtual ~BinaryOperationNode() override = default;
  virtual Value evaluate(Context& ctx) const override {
    auto lhs = left->evaluate(ctx).v;
    auto rhs = right->evaluate(ctx).v;

    return std::visit(
      []<class A, class B>(const A& a, const B& b) -> Value {
        if constexpr (requires { F{}(a, b); })
          return {F{}(a, b)};
        else
          throw std::runtime_error(
            std::format(
              "Operator '{}' does not support types '{}' and '{}'",
              name_type<F>(),
              name_type<A>(),
              name_type<B>()
            )
          );
      },
      lhs,
      rhs
    );
  }
};

struct LetNode final : public Node {
  static constexpr std::string_view op_name = "let ";
  const std::string name;

  explicit LetNode(std::string_view _name) :
    name([](auto _name) {
      if (!sv::is_variable_name(_name))
        throw std::invalid_argument(
          std::format("'{}' is not a valid variable name", _name)
        );
      return _name;
    }(_name.substr(op_name.size()))) {}
  virtual ~LetNode() override = default;
  virtual Value evaluate(Context& ctx) const override {
    auto [it, inserted] = ctx.insert({name, Value{std::monostate{}}});
    if (!inserted)
      throw std::runtime_error(
        std::format("Variable '{}' already declared", name)
      );
    return Value{Ref{it}};
  }
};

struct RefNode final : public Node {
  static constexpr std::string_view op_name = "&";
  const std::string name;

  explicit RefNode(std::string_view _name) :
    name([](auto _name) {
      if (!sv::is_variable_name(_name))
        throw std::invalid_argument(
          std::format("'{}' is not a valid variable name", _name)
        );
      return _name;
    }(_name.substr(op_name.size()))) {}
  virtual ~RefNode() override = default;
  virtual Value evaluate(Context& ctx) const override {
    if (auto it = ctx.find(name); it != ctx.end())
      return {Ref{it}};
    throw std::runtime_error(std::format("{} was not declared", name));
  }
};

struct DerefNode final : public Node {
  static constexpr std::string_view op_name = "*";
  NodePtr expr;

  explicit DerefNode(std::string_view expr) :
    expr(parse_expression(expr.substr(op_name.size()))) {}
  virtual ~DerefNode() override = default;
  virtual Value evaluate(Context& ctx) const override {
    return std::visit(
      []<class T>(const T& arg) -> Value {
        if constexpr (requires { *arg.ref; })
          return arg.ref->second;
        else
          throw std::runtime_error(
            std::format(
              "Operator '{}' does not support type '{}'",
              op_name,
              name_type<T>()
            )
          );
      },
      expr->evaluate(ctx).v
    );
  }
};

enum class Associativity : bool {
  Left,
  Right,
};

using NodeFactory = NodePtr (*)(std::string_view, std::size_t);
using OperationFinder = std::size_t (*)(std::string_view);

struct OperationInfo {
  NodeFactory factory;
  OperationFinder finder;
};

int parenthesis_count(std::string_view expr) {
  return std::ranges::fold_left(expr, 0, [](int acc, char c) {
    if (c == ')')
      return acc - 1;
    if (c == '(')
      return acc + 1;
    return acc;
  });
}

auto find_operator(std::string_view expr, std::string_view op_name) {
  std::size_t pos = expr.find(op_name);
  while (pos != std::string_view::npos) {
    if (parenthesis_count(expr.substr(0, pos)) == 0)
      break;
    pos = expr.find(op_name, pos + 1);
  }
  return pos;
}

auto rfind_operator(std::string_view expr, std::string_view op_name) {
  std::size_t pos = expr.rfind(op_name);
  while (pos != std::string_view::npos) {
    if (parenthesis_count(expr.substr(0, pos)) == 0)
      break;
    pos = expr.rfind(op_name, pos - 1);
  }
  return pos;
}

std::size_t skip_unary_operator(
  std::string_view expr, std::size_t pos, std::string_view name
) {
  while (pos != std::string_view::npos) {
    expr = sv::rtrim(expr.substr(0, pos));
    if (!expr.empty() && (expr.back() == ')' || sv::is_alnum(expr.back())) &&
        parenthesis_count(expr) == 0)
      break;

    pos = rfind_operator(expr, name);
  }
  return pos;
}

template <class F, Associativity A>
static constexpr OperationInfo binary_info = {
  .factory = [](std::string_view expr, std::size_t pos) -> NodePtr {
    return std::make_unique<BinaryOperationNode<F>>(expr, pos);
  },
  .finder = [](std::string_view expr) -> std::size_t {
    std::size_t pos;
    static constexpr auto op_name = BinaryOperationNode<F>::op_name;
    if constexpr (A == Associativity::Right)
      pos = find_operator(expr, op_name);
    else
      pos = rfind_operator(expr, op_name);
    if constexpr (std::is_same_v<F, std::minus<>> ||
                  std::is_same_v<F, std::multiplies<>>)
      pos = skip_unary_operator(expr, pos, op_name);
    return pos;
  }
};

static constexpr OperationInfo let_info = {
  .factory = [](std::string_view expr, std::size_t) -> NodePtr {
    return std::make_unique<LetNode>(expr);
  },
  .finder = [](std::string_view expr) -> std::size_t {
    static constexpr auto op_name = LetNode::op_name;
    return expr.starts_with(op_name) ? 0 : std::string_view::npos;
  }
};
static constexpr OperationInfo ref_info = {
  .factory = [](std::string_view expr, std::size_t) -> NodePtr {
    return std::make_unique<RefNode>(expr);
  },
  .finder = [](std::string_view expr) -> std::size_t {
    static constexpr auto op_name = RefNode::op_name;
    return expr.starts_with(op_name) ? 0 : std::string_view::npos;
  }
};
static constexpr OperationInfo deref_info = {
  .factory = [](std::string_view expr, std::size_t) -> NodePtr {
    return std::make_unique<DerefNode>(expr);
  },
  .finder = [](std::string_view expr) -> std::size_t {
    static constexpr auto op_name = DerefNode::op_name;
    return expr.starts_with(op_name) ? 0 : std::string_view::npos;
  }
};

NodePtr make_operator_node(std::string_view expr) {
  static constexpr std::array operations = {
    binary_info<Assign, Associativity::Right>,
    binary_info<std::logical_or<>, Associativity::Left>,
    binary_info<std::logical_and<>, Associativity::Left>,
    binary_info<std::greater_equal<>, Associativity::Left>,
    binary_info<std::less_equal<>, Associativity::Left>,
    binary_info<std::greater<>, Associativity::Left>,
    binary_info<std::less<>, Associativity::Left>,
    binary_info<std::equal_to<>, Associativity::Left>,
    binary_info<std::not_equal_to<>, Associativity::Left>,
    binary_info<std::plus<>, Associativity::Left>,
    binary_info<std::minus<>, Associativity::Left>,
    binary_info<std::multiplies<>, Associativity::Left>,
    binary_info<std::divides<>, Associativity::Left>,
    binary_info<std::modulus<>, Associativity::Left>,
    ref_info,
    deref_info,
    let_info,
  };
  for (const auto& op : operations)
    if (auto pos = op.finder(expr); pos != std::string_view::npos)
      return op.factory(expr, pos);
  throw std::invalid_argument(std::format("Invalid expression: '{}'", expr));
}

bool encapsulated(std::string_view expr) {
  if (!expr.starts_with('(') || !expr.ends_with(')'))
    return false;

  int depth = 1;
  for (char c : expr.substr(1, expr.size() - 2)) {
    if (c == '(')
      ++depth;
    else if (c == ')')
      --depth;
    if (depth == 0)
      return false;
  }
  return true;
}
} // namespace

Value parse_value(std::string_view expr) {
  if (expr == "()")
    return {std::monostate{}};
  if (expr == "true")
    return {true};
  if (expr == "false")
    return {false};

  if (expr.starts_with('"') && expr.find('"', 1) + 1 == expr.size())
    return {std::string(expr.substr(1, expr.size() - 2))};

  {
    int i{};
    const auto [ptr, ec] =
      std::from_chars(expr.data(), expr.data() + expr.size(), i);
    if (ec == std::errc() && ptr == expr.data() + expr.size())
      return {i};
  }

  {
    float f{};
    const auto [ptr, ec] =
      std::from_chars(expr.data(), expr.data() + expr.size(), f);
    if (ec == std::errc() && ptr == expr.data() + expr.size())
      return {f};
  }

  throw std::invalid_argument(std::format("Invalid literal: '{}'", expr));
}

NodePtr parse_expression(std::string_view expr) {
  expr = sv::trim(expr);
  while (expr.size() > 2 && encapsulated(expr))
    expr = sv::trim(expr.substr(1, expr.size() - 2));

  try {
    return std::make_unique<LiteralNode>(parse_value(expr));
  } catch (const std::invalid_argument&) {}
  try {
    return std::make_unique<VariableNode>(expr);
  } catch (const std::invalid_argument&) {}
  return make_operator_node(expr);
}
