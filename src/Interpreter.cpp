#include "Interpreter.hpp"
#include <array>
#include <concepts>
#include <format>
#include <functional>
#include <stdexcept>
#include <string_view>
#include <utility>

namespace {

template <class T>
constexpr std::string_view name_type() {
  if constexpr (std::same_as<T, int>)
    return "int";
  else if constexpr (std::same_as<T, float>)
    return "float";
  else if constexpr (std::same_as<T, bool>)
    return "bool";
  else if constexpr (std::same_as<T, std::string>)
    return "string";
  else if constexpr (std::same_as<T, std::logical_or<>>)
    return "||";
  else if constexpr (std::same_as<T, std::logical_and<>>)
    return "&&";
  else if constexpr (std::same_as<T, std::greater_equal<>>)
    return ">=";
  else if constexpr (std::same_as<T, std::less_equal<>>)
    return "<=";
  else if constexpr (std::same_as<T, std::greater<>>)
    return ">";
  else if constexpr (std::same_as<T, std::less<>>)
    return "<";
  else if constexpr (std::same_as<T, std::equal_to<>>)
    return "==";
  else if constexpr (std::same_as<T, std::not_equal_to<>>)
    return "!=";
  else if constexpr (std::same_as<T, std::plus<>>)
    return "+";
  else if constexpr (std::same_as<T, std::minus<>>)
    return "-";
  else if constexpr (std::same_as<T, std::multiplies<>>)
    return "*";
  else if constexpr (std::same_as<T, std::divides<>>)
    return "/";
  else if constexpr (std::same_as<T, std::modulus<>>)
    return "%";
  else
    static_assert(false, "Unexpected type");
}

using SyntaxTree = std::unique_ptr<Node>;
struct ValueNode final : public Node {
  const Value value;

  ValueNode(Value _value) : value{std::move(_value)} {}
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
    } catch (std::out_of_range& oor) {
      throw name;
    }
  }
};

template <class F>
struct OperationNode final : public Node {
  static constexpr auto operation = []<class A, class B>(A a, B b) -> Value {
    if constexpr (requires { F{}(a, b); })
      return F{}(a, b);
    else {
      std::string_view type1;
      std::string_view type2;
      std::string_view op;

      throw std::runtime_error(
        std::format(
          "Types {:?} and {:?} unsupported for operator {:?}",
          name_type<A>(),
          name_type<B>(),
          name_type<F>()
        )
      );
    }
  };
  const SyntaxTree left;
  const SyntaxTree right;

  OperationNode(SyntaxTree&& _left, SyntaxTree&& _right) :
    left(std::move(_left)),
    right(std::move(_right)) {}
  virtual ~OperationNode() override = default;
  virtual Value evaluate(const Context& context) const override {
    return std::visit(
      operation, left->evaluate(context), right->evaluate(context)
    );
  }
};

using NodeFactory = SyntaxTree (*)(SyntaxTree, SyntaxTree);

struct OperationInfo {
  NodeFactory factory;
  std::string_view name;

  constexpr OperationInfo() : factory(nullptr), name("") {}
  template <class F>
  constexpr OperationInfo(F) :
    factory([](SyntaxTree left, SyntaxTree right) -> SyntaxTree {
      return std::make_unique<OperationNode<F>>(
        std::move(left), std::move(right)
      );
    }),
    name(name_type<F>()) {}
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

SyntaxTree make_tree(std::string_view expr) {
  while (expr.front() == '(' && expr.back() == ')' && balanced(expr))
    expr = expr.substr(1, expr.size() - 2);

  auto [pos, op] = find_lowest_op(expr);
  if (pos == std::string::npos) {
    if (expr.front() == '$')
      return std::make_unique<VariableNode>(std::string(expr.substr(1)));
    return std::make_unique<ValueNode>(parse_value(std::move(expr)));
  }

  auto left_tree = make_tree(expr.substr(0, pos));
  auto right_tree = make_tree(expr.substr(pos + op.name.size()));
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

SyntaxTree parse_expression(std::string_view expression) {
  std::string formatted_expression{expression};
  std::erase(formatted_expression, ' ');
  return make_tree(formatted_expression);
}
