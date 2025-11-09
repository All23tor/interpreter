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
  NameType<int, CompStr{"int"}>,
  NameType<float, CompStr{"float"}>,
  NameType<bool, CompStr{"bool"}>,
  NameType<std::string, CompStr{"string"}>,
  NameType<std::monostate, CompStr{"unit"}>,
  NameType<std::logical_or<>, CompStr{"||"}>,
  NameType<std::logical_and<>, CompStr{"&&"}>,
  NameType<std::greater_equal<>, CompStr{">="}>,
  NameType<std::less_equal<>, CompStr{"<="}>,
  NameType<std::greater<>, CompStr{">"}>,
  NameType<std::less<>, CompStr{"<"}>,
  NameType<std::equal_to<>, CompStr{"=="}>,
  NameType<std::not_equal_to<>, CompStr{"!="}>,
  NameType<std::plus<>, CompStr{"+"}>,
  NameType<std::minus<>, CompStr{"-"}>,
  NameType<std::multiplies<>, CompStr{"*"}>,
  NameType<std::divides<>, CompStr{"/"}>,
  NameType<std::modulus<>, CompStr{"%"}>>;
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

} // namespace sv

using NodePtr = std::unique_ptr<Node>;
struct LiteralNode final : public Node {
  const Value value;

  LiteralNode(Value _value) : value{std::move(_value)} {}
  virtual ~LiteralNode() override final = default;
  virtual Value evaluate(Context&) const override final {
    return value;
  }
};

struct VariableNode final : public Node {
  const std::string name;

  VariableNode(std::string_view _name) :
    name([](auto _name) {
      if (_name.empty())
        throw std::invalid_argument("Expected something, got nothing");

      bool is_variable_name = !sv::is_digit(_name.front()) &&
        std::ranges::all_of(_name, [](unsigned char c) {
          return sv::is_alnum(c) || c == '_';
        });
      if (!is_variable_name)
        throw std::invalid_argument(
          std::format("{} is not a valid variable name", _name)
        );
      return _name;
    }(_name)) {}
  virtual ~VariableNode() override final = default;
  virtual Value evaluate(Context& ctx) const override final {
    try {
      return ctx.at(name);
    } catch (const std::out_of_range&) {
      throw std::runtime_error(std::format("{} was not declared", name));
    }
  }
};

template <class F>
struct BinaryOperationNode final : public Node {
  static constexpr auto op_name = name_type<F>();
  NodePtr left;
  NodePtr right;

  BinaryOperationNode(std::string_view expr, std::size_t pos) :
    left(parse_expression(expr.substr(0, pos))),
    right(parse_expression(expr.substr(pos + op_name.size()))) {}
  virtual ~BinaryOperationNode() override = default;
  virtual Value evaluate(Context& ctx) const override {
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
      left->evaluate(ctx).v,
      right->evaluate(ctx).v
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

std::size_t skip_unary_minus(std::string_view expr, std::size_t pos) {
  while (pos != std::string_view::npos) {
    expr = sv::rtrim(expr.substr(0, pos));
    if (!expr.empty() && (expr.back() == ')' || sv::is_alnum(expr.back())) &&
        parenthesis_count(expr) == 0)
      break;

    pos = rfind_operator(expr, "-");
  }
  return pos;
}

template <class F, Associativity A>
static constexpr OperationInfo binary_info() {
  return {
    .factory = ([](std::string_view expr, std::size_t pos) -> NodePtr {
      return std::make_unique<BinaryOperationNode<F>>(expr, pos);
    }),
    .finder = ([](std::string_view expr) {
      std::size_t pos;
      static constexpr auto op_name = name_type<F>();
      if constexpr (A == Associativity::Right)
        pos = find_operator(expr, op_name);
      else
        pos = rfind_operator(expr, op_name);
      if constexpr (std::is_same_v<F, std::minus<>>)
        pos = skip_unary_minus(expr, pos);
      return pos;
    })
  };
}

static constexpr std::array operations = {
  binary_info<std::logical_or<>, Associativity::Left>(),
  binary_info<std::logical_and<>, Associativity::Left>(),
  binary_info<std::greater_equal<>, Associativity::Left>(),
  binary_info<std::less_equal<>, Associativity::Left>(),
  binary_info<std::greater<>, Associativity::Left>(),
  binary_info<std::less<>, Associativity::Left>(),
  binary_info<std::equal_to<>, Associativity::Left>(),
  binary_info<std::not_equal_to<>, Associativity::Left>(),
  binary_info<std::plus<>, Associativity::Left>(),
  binary_info<std::minus<>, Associativity::Left>(),
  binary_info<std::multiplies<>, Associativity::Left>(),
  binary_info<std::divides<>, Associativity::Left>(),
  binary_info<std::modulus<>, Associativity::Left>()
};

bool encapsulating_parenthesis(std::string_view expr) {
  if (expr.size() == 2 || !expr.starts_with('(') || !expr.ends_with(')'))
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

  if (expr.size() >= 2 && expr.front() == '"' && expr.back() == '"')
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
  while (encapsulating_parenthesis(expr))
    expr = sv::trim(expr.substr(1, expr.size() - 2));

  for (const auto& op : operations)
    if (auto pos = op.finder(expr); pos != std::string_view::npos)
      return op.factory(expr, pos);

  try {
    return std::make_unique<LiteralNode>(parse_value(expr));
  } catch (const std::invalid_argument&) {
    return std::make_unique<VariableNode>(expr);
  }
}
