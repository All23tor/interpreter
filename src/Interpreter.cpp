#include "Interpreter.hpp"
#include <array>
#include <format>
#include <functional>
#include <stdexcept>
#include <string_view>
#include <utility>

namespace {
using NodePtr = std::unique_ptr<Node>;

template <class T>
constexpr std::string_view name_type;
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
    try {
      return ctx.at(name);
    } catch (std::out_of_range& oor) {
      throw name;
    }
  }
};

template <class F>
struct OperationNode final : public Node {
  NodePtr left;
  NodePtr right;

  OperationNode(NodePtr&& _left, NodePtr&& _right) :
    left(std::move(_left)),
    right(std::move(_right)) {}
  virtual ~OperationNode() override = default;
  virtual Value evaluate(const Context& ctx) const override {
    return std::visit(
      []<class A, class B>(A a, B b) -> Value {
        if constexpr (requires { F{}(a, b); })
          return F{}(a, b);
        else
          throw std::runtime_error(
            std::format(
              "Types {:?} and {:?} unsupported for operator {:?}",
              name_type<A>,
              name_type<B>,
              name_type<F>
            )
          );
      },
      left->evaluate(ctx),
      right->evaluate(ctx)
    );
  }
};

using NodeFactory = NodePtr (*)(NodePtr, NodePtr);

struct OperationInfo {
  NodeFactory factory;
  std::string_view name;

  constexpr OperationInfo() : factory(nullptr), name("") {}
  template <class F>
  constexpr OperationInfo(F) :
    factory([](NodePtr left, NodePtr right) -> NodePtr {
      return std::make_unique<OperationNode<F>>(
        std::move(left), std::move(right)
      );
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

std::pair<std::size_t, OperationInfo> find_lowest_op(std::string_view expr) {
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

    return {pos, operation};
  }
  return {std::string_view::npos, {}};
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

NodePtr make_tree(std::string_view expr) {
  while (expr.starts_with('(') && expr.ends_with(')') && balanced(expr))
    expr = expr.substr(1, expr.size() - 2);

  auto [pos, op] = find_lowest_op(expr);
  if (pos == std::string::npos) {
    if (expr.starts_with('$'))
      return std::make_unique<VariableNode>(std::string(expr.substr(1)));
    return std::make_unique<LiteralNode>(parse_value(expr));
  }

  auto left_tree = make_tree(expr.substr(0, pos));
  auto right_tree = make_tree(expr.substr(pos + op.name.size()));
  return op.factory(std::move(left_tree), std::move(right_tree));
}
} // namespace

Value parse_value(std::string_view expr) {
  if (expr == "true")
    return true;
  if (expr == "false")
    return false;
  if (expr.contains('.'))
    return std::stof(std::string(expr));
  if (expr.size() >= 2 && expr.starts_with('"') && expr.ends_with('"'))
    return std::string(expr.substr(1, expr.size() - 2));
  return std::stoi(std::string(expr));
}

NodePtr parse_expression(std::string_view expression) {
  std::string formatted_expression{expression};
  std::erase(formatted_expression, ' ');
  return make_tree(formatted_expression);
}
