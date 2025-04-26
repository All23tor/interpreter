#include "Interpreter.hpp"
#include <type_traits>

namespace {
template <class ValueType>
requires std::is_constructible_v<Value, ValueType>
struct Number final : public Node {
  const ValueType number;

  Number(const ValueType& _number) : number{_number} {}
  Number(ValueType&& _number) : number{std::move(_number)} {}
  ~Number() override = default;
  Value evaluate(const Context&) const override {
    return number;
  }
};

using Bool = Number<bool>;
using Int = Number<int>;
using Float = Number<float>;
using String = Number<std::string>;

struct Variable final : public Node {
  const std::string name;

  Variable(std::string _name) : name{std::move(_name)} {}
  ~Variable() override = default;
  Value evaluate(const Context& context) const override {
    return context.at(name);
  }
};

template <auto Visitor>
struct Operation final : public Node {
  const std::unique_ptr<Node> left;
  const std::unique_ptr<Node> right;

  Operation(std::unique_ptr<Node>&& _left, std::unique_ptr<Node>&& _right) :
      left(std::move(_left)),
      right(std::move(_right)) {}
  virtual ~Operation() override = default;
  Value evaluate(const Context& context) const override {
    return std::visit(Visitor, left->evaluate(context),
                      right->evaluate(context));
  }
};

using Or = Operation<[](auto&& a, auto&& b) -> Value {
  using A = std::remove_cvref_t<decltype(a)>;
  using B = std::remove_cvref_t<decltype(b)>;
  constexpr bool aStr = std::is_same_v<A, std::string>;
  constexpr bool bStr = std::is_same_v<B, std::string>;
  if constexpr (aStr || bStr)
    throw;
  else
    return a || b;
}>;
using And = Operation<[](auto&& a, auto&& b) -> Value {
  using A = std::remove_cvref_t<decltype(a)>;
  using B = std::remove_cvref_t<decltype(b)>;
  constexpr bool aStr = std::is_same_v<A, std::string>;
  constexpr bool bStr = std::is_same_v<B, std::string>;
  if constexpr (aStr || bStr)
    throw;
  else
    return a && b;
}>;
using More = Operation<[](auto&& a, auto&& b) -> Value {
  using A = std::remove_cvref_t<decltype(a)>;
  using B = std::remove_cvref_t<decltype(b)>;
  constexpr bool aStr = std::is_same_v<A, std::string>;
  constexpr bool bStr = std::is_same_v<B, std::string>;
  if constexpr (aStr != bStr)
    throw;
  else
    return (a > b);
}>;
using Less = Operation<[](auto&& a, auto&& b) -> Value {
  using A = std::remove_cvref_t<decltype(a)>;
  using B = std::remove_cvref_t<decltype(b)>;
  constexpr bool aStr = std::is_same_v<A, std::string>;
  constexpr bool bStr = std::is_same_v<B, std::string>;
  if constexpr (aStr != bStr)
    throw;
  else
    return a < b;
}>;
using Equals = Operation<[](auto&& a, auto&& b) -> Value {
  using A = std::remove_cvref_t<decltype(a)>;
  using B = std::remove_cvref_t<decltype(b)>;
  constexpr bool aStr = std::is_same_v<A, std::string>;
  constexpr bool bStr = std::is_same_v<B, std::string>;
  if constexpr (aStr != bStr)
    throw;
  else
    return a == b;
}>;
using Plus = Operation<[](auto&& a, auto&& b) -> Value {
  using A = std::remove_cvref_t<decltype(a)>;
  using B = std::remove_cvref_t<decltype(b)>;
  constexpr bool aStr = std::is_same_v<A, std::string>;
  constexpr bool bStr = std::is_same_v<B, std::string>;
  if constexpr (aStr != bStr)
    throw;
  else
    return a + b;
}>;
using Minus = Operation<[](auto&& a, auto&& b) -> Value {
  using A = std::remove_cvref_t<decltype(a)>;
  using B = std::remove_cvref_t<decltype(b)>;
  constexpr bool aStr = std::is_same_v<A, std::string>;
  constexpr bool bStr = std::is_same_v<B, std::string>;
  if constexpr (aStr || bStr)
    throw;
  else
    return a - b;
}>;
using Times = Operation<[](auto&& a, auto&& b) -> Value {
  using A = std::remove_cvref_t<decltype(a)>;
  using B = std::remove_cvref_t<decltype(b)>;
  constexpr bool aStr = std::is_same_v<A, std::string>;
  constexpr bool bStr = std::is_same_v<B, std::string>;
  if constexpr (aStr || bStr)
    throw;
  else
    return a * b;
}>;
using Over = Operation<[](auto&& a, auto&& b) -> Value {
  using A = std::remove_cvref_t<decltype(a)>;
  using B = std::remove_cvref_t<decltype(b)>;
  constexpr bool aStr = std::is_same_v<A, std::string>;
  constexpr bool bStr = std::is_same_v<B, std::string>;
  if constexpr (aStr || bStr)
    throw;
  else
    return a / b;
}>;

static bool isOperator(char c) {
  return c == '+' || c == '-' || c == '*' || c == '/' || c == '|' || c == '&' ||
         c == '>' || c == '<' || c == '=';
}

bool isLower(char op1, char op2) {
  static constexpr int (*getPrecedence)(char) = [](char op) {
    switch (op) {
    case '|':
      return 1;
    case '&':
      return 2;
    case '>':
    case '<':
      return 3;
    case '=':
      return 4;
    case '+':
    case '-':
      return 5;
    case '/':
    case '*':
      return 6;
    default:
      return 0;
    }
  };

  return getPrecedence(op1) <= getPrecedence(op2);
}

std::size_t findLowest(std::string_view expr) {
  char currentOp = '\0';
  auto opPosition = std::string::npos;

  bool expectUnary = true;
  int parenthesis = 0;

  for (size_t i = 0; i < expr.length(); ++i) {
    char c = expr[i];

    if (c == '(') {
      ++parenthesis;
      expectUnary = true;
      continue;
    } else if (c == ')') {
      --parenthesis;
      expectUnary = false;
      continue;
    }

    if (!isOperator(c) || parenthesis != 0) {
      expectUnary = false;
      continue;
    }

    if (c == '-' && expectUnary)
      continue;

    if (!currentOp || isLower(c, currentOp)) {
      currentOp = c;
      opPosition = i;
    }

    expectUnary = true;
  }

  return opPosition;
};

bool balancedParenthesis(std::string& expression) {
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

std::unique_ptr<Node> makeTree(std::string&& expression,
                               std::set<std::string>& vars) {
  while (expression.front() == '(' && expression.back() == ')')
    if (balancedParenthesis(expression))
      expression = expression.substr(1, expression.length() - 2);
    else
      break;

  auto opPos = findLowest(expression);

  if (opPos == std::string::npos) {
    if (expression.front() == '$') {
      expression.erase(0, 1);
      vars.insert(expression);
      return std::make_unique<Variable>(std::move(expression));
    } else if (expression.front() == '"') {
      expression.pop_back();
      expression.erase(0, 1);
      return std::make_unique<String>(std::move(expression));
    } else if (expression == "true")
      return std::make_unique<Bool>(true);
    else if (expression == "false")
      return std::make_unique<Bool>(false);
    else if (expression.contains('.'))
      return std::make_unique<Float>(std::stof(expression));
    else
      return std::make_unique<Int>(std::stoi(expression));
  }

  auto leftNode = makeTree(expression.substr(0, opPos), vars);
  auto rightNode = makeTree(expression.substr(opPos + 1), vars);

  switch (expression[opPos]) {
  case '|':
    return std::make_unique<Or>(std::move(leftNode), std::move(rightNode));
  case '&':
    return std::make_unique<And>(std::move(leftNode), std::move(rightNode));
  case '>':
    return std::make_unique<More>(std::move(leftNode), std::move(rightNode));
  case '<':
    return std::make_unique<Less>(std::move(leftNode), std::move(rightNode));
  case '=':
    return std::make_unique<Equals>(std::move(leftNode), std::move(rightNode));
  case '+':
    return std::make_unique<Plus>(std::move(leftNode), std::move(rightNode));
  case '-':
    return std::make_unique<Minus>(std::move(leftNode), std::move(rightNode));
  case '*':
    return std::make_unique<Times>(std::move(leftNode), std::move(rightNode));
  case '/':
    return std::make_unique<Over>(std::move(leftNode), std::move(rightNode));
  default:
    return nullptr;
  }
}
} // namespace

ParseResult parseExpression(std::string expression) {
  std::erase(expression, ' ');
  std::set<std::string> variables;
  auto tree = makeTree(std::move(expression), variables);
  return {std::move(tree), std::move(variables)};
}
