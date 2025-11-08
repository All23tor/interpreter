#include "Interpreter.hpp"
#include <algorithm>
#include <array>
#include <format>
#include <functional>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <type_traits>
#include <utility>

namespace {
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

  VariableNode(std::string _name) : name{std::move(_name)} {}
  virtual ~VariableNode() override final = default;
  virtual Value evaluate(const Context& ctx) const override final {
    return ctx.at(name);
  }
};

NodePtr make_tree(std::string_view expr);

template <class F>
struct BinaryOperationNode final : public Node {
  static constexpr auto op_name = name_type<F>;
  NodePtr left;
  NodePtr right;

  BinaryOperationNode(std::string_view expr, std::size_t pos) :
    left(make_tree(expr.substr(0, pos))),
    right(make_tree(expr.substr(pos + op_name.size()))) {}
  virtual ~BinaryOperationNode() override = default;
  virtual Value evaluate(const Context& ctx) const override {
    return std::visit(
      []<class A, class B>(const A& a, const B& b) -> Value {
        if constexpr (requires { F{}(a, b); })
          return {F{}(a, b)};
        else
          throw std::runtime_error(
            std::format(
              "Operator {} does not support types {} and {}",
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

struct OperationInfo {
  NodeFactory factory;
  std::string_view name;

  template <class F>
  constexpr OperationInfo(F) :
    factory([](std::string_view expr, std::size_t pos) -> NodePtr {
      return std::make_unique<BinaryOperationNode<F>>(expr, pos);
    }),
    name(name_type<F>) {}
};

static constexpr std::array<OperationInfo, 13> operations = {
  std::logical_or{},
  std::logical_and{},
  std::greater_equal{},
  std::less_equal{},
  std::greater{},
  std::less{},
  std::equal_to{},
  std::not_equal_to{},
  std::plus{},
  std::minus{},
  std::multiplies{},
  std::divides{},
  std::modulus{},
};

std::size_t handle_unary_minus(std::string_view expr, std::size_t pos) {
  if (pos == 0)
    pos = expr.find("-", 1);
  while (pos != std::string_view::npos && !std::isalnum(expr[pos - 1]))
    pos = expr.find("-", pos + 1);
  return pos;
}

struct FoundOperator {
  std::size_t pos;
  OperationInfo operation;
};

std::optional<FoundOperator> find_lowest_op(std::string_view expr) {
  for (const auto& operation : operations) {
    auto pos = expr.find(operation.name);
    if (operation.name == "-")
      pos = handle_unary_minus(expr, pos);

    if (pos == std::string_view::npos)
      continue;

    int parenCount = 0;
    for (char c : expr.substr(0, pos))
      if (c == ')')
        parenCount--;
      else if (c == '(')
        parenCount++;

    if (parenCount != 0)
      continue;

    return FoundOperator{pos, operation};
  }
  return std::nullopt;
}

bool balanced(std::string_view expression) {
  int depth = 0;
  for (char c : expression) {
    if (c == '(')
      ++depth;
    else if (c == ')')
      --depth;
    if (depth < 0)
      return false;
  }
  return depth == 0;
}

bool variable_name(std::string_view name) {
  static constexpr std::array<std::string_view, 2> reserved = {"false", "true"};
  if (std::ranges::contains(reserved, name))
    return false;
  if (name.empty() || std::isdigit(static_cast<unsigned char>(name.front())))
    return false;
  return std::ranges::all_of(name, [](unsigned char c) {
    return std::isalnum(c) || c == '_';
  });
}

NodePtr make_tree(std::string_view expr) {
  while (expr.starts_with('(') && expr.ends_with(')') && balanced(expr))
    expr = expr.substr(1, expr.size() - 2);

  auto result = find_lowest_op(expr);
  if (result) {
    auto [pos, op] = *result;
    return op.factory(expr, pos);
  }

  if (variable_name(expr))
    return std::make_unique<VariableNode>(std::string(expr));
  return std::make_unique<LiteralNode>(parse_value(expr));
}
} // namespace

Value parse_value(std::string_view expr) {
  if (expr == "true")
    return Value{true};
  if (expr == "false")
    return Value{false};
  if (expr.contains('.'))
    return Value{std::stof(std::string(expr))};
  if (expr.size() >= 2 && expr.starts_with('"') && expr.ends_with('"'))
    return Value{std::string(expr.substr(1, expr.size() - 2))};
  return Value{std::stoi(std::string(expr))};
}

NodePtr parse_expression(std::string_view expression) {
  std::string formatted_expression{expression};
  std::erase(formatted_expression, ' ');
  return make_tree(formatted_expression);
}
