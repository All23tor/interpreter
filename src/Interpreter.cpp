#include "Interpreter.hpp"
#include <algorithm>
#include <array>
#include <charconv>
#include <format>
#include <functional>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <type_traits>
#include <utility>

namespace {
namespace sr = std::ranges;

template <class T>
constexpr std::string_view name_type = [] {
  static_assert(false, "name_type<T> is not specialized for this type");
}();
template <>
constexpr std::string_view name_type<int> = "int";
template <>
constexpr std::string_view name_type<float> = "float";
template <>
constexpr std::string_view name_type<bool> = "bool";
template <>
constexpr std::string_view name_type<std::string> = "string";
template <>
constexpr std::string_view name_type<std::logical_or<>> = "||";
template <>
constexpr std::string_view name_type<std::logical_and<>> = "&&";
template <>
constexpr std::string_view name_type<std::greater_equal<>> = ">=";
template <>
constexpr std::string_view name_type<std::less_equal<>> = "<=";
template <>
constexpr std::string_view name_type<std::greater<>> = ">";
template <>
constexpr std::string_view name_type<std::less<>> = "<";
template <>
constexpr std::string_view name_type<std::equal_to<>> = "==";
template <>
constexpr std::string_view name_type<std::not_equal_to<>> = "!=";
template <>
constexpr std::string_view name_type<std::plus<>> = "+";
template <>
constexpr std::string_view name_type<std::minus<>> = "-";
template <>
constexpr std::string_view name_type<std::multiplies<>> = "*";
template <>
constexpr std::string_view name_type<std::divides<>> = "/";
template <>
constexpr std::string_view name_type<std::modulus<>> = "%";

using NodePtr = std::unique_ptr<Node>;
struct LiteralNode final : public Node {
  const Value value;

  LiteralNode(Value _value) : value{std::move(_value)} {}
  virtual ~LiteralNode() override final = default;
  virtual Value evaluate(const Context&) const override final {
    return value;
  }
};

struct VariableNode final : public Node {
  const std::string name;

  VariableNode(std::string_view _name) : name{_name} {}
  virtual ~VariableNode() override final = default;
  virtual Value evaluate(const Context& ctx) const override final {
    return ctx.at(name);
  }
};

template <class F>
struct BinaryOperationNode final : public Node {
  static constexpr auto op_name = name_type<F>;
  NodePtr left;
  NodePtr right;

  BinaryOperationNode(std::string_view expr, std::size_t pos) :
    left(parse_expression(expr.substr(0, pos))),
    right(parse_expression(expr.substr(pos + op_name.size()))) {}
  virtual ~BinaryOperationNode() override = default;
  virtual Value evaluate(const Context& ctx) const override {
    return std::visit(
      []<class A, class B>(const A& a, const B& b) -> Value {
        if constexpr (requires { F{}(a, b); })
          return {F{}(a, b)};
        else
          throw std::runtime_error(
            std::format(
              "Operator '{}' does not support types '{}' and '{}'",
              name_type<F>,
              name_type<A>,
              name_type<B>
            )
          );
      },
      left->evaluate(ctx).v,
      right->evaluate(ctx).v
    );
  }
};

using NodeFactory = NodePtr (*)(std::string_view, std::size_t);

enum class Associativity : bool {
  Left,
  Right,
};

struct OperationInfo {
  NodeFactory factory;
  std::string_view name;
  Associativity associativity;

  template <class F>
  constexpr OperationInfo(F, Associativity assoc) :
    factory([](std::string_view expr, std::size_t pos) -> NodePtr {
      return std::make_unique<BinaryOperationNode<F>>(expr, pos);
    }),
    name(name_type<F>),
    associativity(assoc) {}
};

static constexpr std::array<OperationInfo, 13> operations = {{
  {std::logical_or{}, Associativity::Left},
  {std::logical_and{}, Associativity::Left},
  {std::greater_equal{}, Associativity::Left},
  {std::less_equal{}, Associativity::Left},
  {std::greater{}, Associativity::Left},
  {std::less{}, Associativity::Left},
  {std::equal_to{}, Associativity::Left},
  {std::not_equal_to{}, Associativity::Left},
  {std::plus{}, Associativity::Left},
  {std::minus{}, Associativity::Left},
  {std::multiplies{}, Associativity::Left},
  {std::divides{}, Associativity::Left},
  {std::modulus{}, Associativity::Left},
}};

// necessary to avoid ugly static_cast<unsigned char> everywhere
constexpr auto is_alnum = [](unsigned char c) { return std::isalnum(c); };
constexpr auto is_digit = [](unsigned char c) { return std::isdigit(c); };
constexpr auto is_space = [](unsigned char c) { return std::isspace(c); };

std::string_view ltrim(std::string_view sv) {
  auto it = sr::find_if_not(sv, is_space);
  return sv.substr(it - sv.begin());
}

std::string_view rtrim(std::string_view sv) {
  auto it = sr::find_last_if_not(sv, is_space).begin();
  return sv.substr(0, (it - sv.begin()) + 1);
}

std::string_view trim(std::string_view sv) {
  return rtrim(ltrim(sv));
}

int parenthesis_count(std::string_view expr) {
  return sr::fold_left(expr, 0, [](int acc, char c) {
    if (c == ')')
      return acc - 1;
    if (c == '(')
      return acc + 1;
    return acc;
  });
}

std::size_t find_operator(std::string_view expr, std::string_view op_name) {
  std::size_t pos = expr.find(op_name);
  while (pos != std::string_view::npos) {
    if (parenthesis_count(expr.substr(0, pos)) == 0)
      break;
    pos = expr.find(op_name, pos + 1);
  }
  return pos;
}

std::size_t rfind_operator(std::string_view expr, std::string_view op_name) {
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
    expr = rtrim(expr.substr(0, pos));
    if (!expr.empty() && (expr.back() == ')' || is_alnum(expr.back())) &&
        parenthesis_count(expr) == 0)
      break;

    pos = rfind_operator(expr, "-");
  }
  return pos;
}

struct FoundOperator {
  std::size_t pos;
  OperationInfo operation;
};

std::optional<FoundOperator> find_lowest_op(std::string_view expr) {
  for (const auto& operation : operations) {
    auto pos = operation.associativity == Associativity::Right ?
      find_operator(expr, operation.name) :
      rfind_operator(expr, operation.name);

    if (operation.name == "-")
      pos = skip_unary_minus(expr, pos);

    if (pos == std::string_view::npos)
      continue;

    return FoundOperator{pos, operation};
  }
  return std::nullopt;
}

bool encapsulating_parenthesis(std::string_view expr) {
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

bool variable_name(std::string_view name) {
  static constexpr std::array<std::string_view, 4> reserved = {
    "false", "true", "inf", "nan"
  };
  if (sr::contains(reserved, name))
    return false;
  if (name.empty() || is_digit(name.front()))
    return false;
  return sr::all_of(name, [](unsigned char c) {
    return is_alnum(c) || c == '_';
  });
}

} // namespace

Value parse_value(std::string_view expr) {
  if (expr.empty())
    throw std::invalid_argument("Expected something, got nothing");

  if (expr == "true")
    return Value{true};
  if (expr == "false")
    return Value{false};

  if (expr.size() >= 2 && expr.front() == '"' && expr.back() == '"')
    return Value{std::string(expr.substr(1, expr.size() - 2))};

  {
    int i{};
    const auto [ptr, ec] =
      std::from_chars(expr.data(), expr.data() + expr.size(), i);
    if (ec == std::errc() && ptr == expr.data() + expr.size())
      return Value{i};
  }

  {
    float f{};
    const auto [ptr, ec] =
      std::from_chars(expr.data(), expr.data() + expr.size(), f);
    if (ec == std::errc() && ptr == expr.data() + expr.size())
      return Value{f};
  }

  throw std::invalid_argument(std::format("Invalid literal: '{}'", expr));
}

NodePtr parse_expression(std::string_view expr) {
  expr = trim(expr);
  while (encapsulating_parenthesis(expr))
    expr = trim(expr.substr(1, expr.size() - 2));

  auto result = find_lowest_op(expr);
  if (result) {
    auto [pos, op] = *result;
    return op.factory(expr, pos);
  }

  if (variable_name(expr))
    return std::make_unique<VariableNode>(expr);
  return std::make_unique<LiteralNode>(parse_value(expr));
}
