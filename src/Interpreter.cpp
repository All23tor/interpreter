#include "Interpreter.hpp"

namespace {
struct ValueNode final : public Node {
  const Value value;

  ValueNode(const Value& _value) : value{_value} {}
  ValueNode(Value&& _value) : value{std::move(_value)} {}
  virtual ~ValueNode() override final = default;
  virtual Value evaluate(const Context&) const override final {
    return value;
  }
};

struct VariableNode final : public Node {
  const std::string name;

  VariableNode(std::string _name) : name{std::move(_name)} {}
  virtual ~VariableNode() override final = default;
  virtual Value evaluate(const Context& context) const override final {
    try {
      return context.at(name);
    } catch (...) {
      throw name;
    }
  }
};

struct UnsuportedOperation : std::runtime_error {
  using std::runtime_error::runtime_error;
};

template <class Func>
struct OperationNode final : public Node {
  static constexpr auto operation = [](auto&& a, auto&& b) -> Value {
    if constexpr (requires { Func{}(a, b); })
      return Func{}(a, b);
    else
      throw std::bad_variant_access();
  };
  const NodePtr left;
  const NodePtr right;

  OperationNode(NodePtr&& _left, NodePtr&& _right) :
      left(std::move(_left)),
      right(std::move(_right)) {}
  virtual ~OperationNode() override final = default;
  virtual Value evaluate(const Context& context) const override final {
    auto lhs = left->evaluate(context);
    auto rhs = right->evaluate(context);
    return std::visit(operation, std::move(lhs), std::move(rhs));
  }
};

using NodeFactory = NodePtr (*)(NodePtr&&, NodePtr&&);
template <typename Func>
constexpr NodeFactory op_factory =
    [](NodePtr&& left, NodePtr&& right) -> NodePtr {
  return std::make_unique<OperationNode<Func>>(std::move(left),
                                               std::move(right));
};

struct OperationInfo {
  std::string_view name;
  NodeFactory factory;
};

constexpr std::array<OperationInfo, 13> operations{{
    {"||", op_factory<std::logical_or<>>},
    {"&&", op_factory<std::logical_and<>>},
    {">=", op_factory<std::greater_equal<>>},
    {"<=", op_factory<std::less_equal<>>},
    {">", op_factory<std::greater<>>},
    {"<", op_factory<std::less<>>},
    {"==", op_factory<std::equal_to<>>},
    {"!=", op_factory<std::not_equal_to<>>},
    {"+", op_factory<std::plus<>>},
    {"-", op_factory<std::minus<>>},
    {"*", op_factory<std::multiplies<>>},
    {"/", op_factory<std::divides<>>},
    {"%", op_factory<std::modulus<>>},
}};

std::size_t handle_unary_minus(std::string_view expr, std::size_t pos) {
  if (pos == 0)
    pos = expr.find("-", 1);
  while (pos != std::string_view::npos && !std::isalnum(expr[pos - 1]))
    pos = expr.find("-", pos + 1);
  return pos;
}

std::pair<std::size_t, OperationInfo> find_lowest(std::string_view expr) {
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

bool balanced_parenthesis(std::string_view expression) {
  int depth = 0;
  for (std::size_t i = 0; i < expression.length() - 1; ++i) {
    char c = expression[i];
    if (c == '(')
      ++depth;
    else if (c == ')')
      --depth;
    if (depth == 0)
      return false;
  }
  return true;
}

NodePtr make_tree(std::string_view expression) {
  while (expression.front() == '(' && expression.back() == ')')
    if (balanced_parenthesis(expression))
      expression = expression.substr(1, expression.size() - 2);
    else
      break;

  auto [pos, op] = find_lowest(expression);
  if (pos == std::string::npos) {
    if (expression.front() == '$')
      return std::make_unique<VariableNode>(std::string(expression.substr(1)));
    return std::make_unique<ValueNode>(parse_value(std::move(expression)));
  }

  auto left_tree = make_tree(expression.substr(0, pos));
  auto right_tree = make_tree(expression.substr(pos + op.name.size()));
  return op.factory(std::move(left_tree), std::move(right_tree));
}
} // namespace

Value parse_value(std::string_view expression) {
  if (expression == "true")
    return true;
  if (expression == "false")
    return false;
  if (expression.contains('.'))
    return std::stof(std::string(expression));
  if (expression.front() == '"')
    return std::string(expression.substr(1, expression.size() - 2));
  return std::stoi(std::string(expression));
}

NodePtr parseExpression(std::string expression) {
  std::erase(expression, ' ');
  return make_tree(expression);
}
