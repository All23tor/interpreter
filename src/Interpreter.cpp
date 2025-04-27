#include "Interpreter.hpp"

namespace {
struct Number final : public Node {
  const Value number;

  Number(const Value& _number) : number{_number} {}
  Number(Value&& _number) : number{std::move(_number)} {}
  ~Number() override = default;
  Value evaluate(const Context&) const override {
    return number;
  }
};

struct Variable final : public Node {
  const std::string name;

  Variable(std::string _name) : name{std::move(_name)} {}
  ~Variable() override = default;
  Value evaluate(const Context& context) const override {
    return context.at(name);
  }
};

template <class Func>
struct Operation final : public Node {
  static constexpr auto Visitor = [](auto&& a, auto&& b) -> Value {
    if constexpr (requires { Func{}(a, b); })
      return Func{}(a, b);
    else
      throw std::bad_variant_access();
  };
  const std::unique_ptr<Node> left;
  const std::unique_ptr<Node> right;

  Operation(std::unique_ptr<Node>&& _left, std::unique_ptr<Node>&& _right) :
      left(std::move(_left)),
      right(std::move(_right)) {}
  virtual ~Operation() override = default;
  Value evaluate(const Context& context) const override {
    auto lhs = left->evaluate(context);
    auto rhs = right->evaluate(context);
    return std::visit(Visitor, std::move(lhs), std::move(rhs));
  }
};

static bool isOperator(char c) {
  return c == '+' || c == '-' || c == '*' || c == '/' || c == '|' || c == '&' ||
         c == '>' || c == '<' || c == '=' || c == '%';
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
    case '%':
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
    } else if (c == ')') {
      --parenthesis;
      expectUnary = false;
    } else if (isOperator(c) && parenthesis == 0) {
      if (c == '-' && expectUnary)
        continue;
      if (!currentOp || isLower(c, currentOp)) {
        currentOp = c;
        opPosition = i;
      }
      expectUnary = true;
    } else {
      expectUnary = false;
    }
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

using NodePtr = std::unique_ptr<Node>;
using NodeFactory = NodePtr (*)(NodePtr&&, NodePtr&&);
template <typename T>
constexpr auto makeOpFactory() {
  return [](NodePtr&& left, NodePtr&& right) -> NodePtr {
    return std::make_unique<Operation<T>>(std::move(left), std::move(right));
  };
}

static inline const auto operatorFactories = []() {
  std::map<char, NodeFactory> table;
  table['|'] = makeOpFactory<std::logical_or<>>();
  table['&'] = makeOpFactory<std::logical_and<>>();
  table['>'] = makeOpFactory<std::greater<>>();
  table['<'] = makeOpFactory<std::less<>>();
  table['='] = makeOpFactory<std::equal_to<>>();
  table['+'] = makeOpFactory<std::plus<>>();
  table['-'] = makeOpFactory<std::minus<>>();
  table['*'] = makeOpFactory<std::multiplies<>>();
  table['/'] = makeOpFactory<std::divides<>>();
  table['%'] = makeOpFactory<std::modulus<>>();
  return table;
}();

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
      expression.erase(expression.begin());
      return std::make_unique<Variable>(std::move(expression));
    }

    Value value;
    if (expression == "true")
      value = true;
    else if (expression == "false")
      value = false;
    else if (expression.contains('.'))
      value = std::stof(expression);
    else if (expression.front() == '"')
      value = expression.substr(1, expression.length() - 2);
    else
      value = std::stoi(expression);
    return std::make_unique<Number>(std::move(value));
  }

  auto leftNode = makeTree(expression.substr(0, opPos), vars);
  auto rightNode = makeTree(expression.substr(opPos + 1), vars);

  auto op = expression[opPos];
  NodeFactory factory = operatorFactories.at(op);
  return factory(std::move(leftNode), std::move(rightNode));
}
} // namespace

ParseResult parseExpression(std::string expression) {
  std::erase(expression, ' ');
  std::set<std::string> variables;
  auto tree = makeTree(std::move(expression), variables);
  return {std::move(tree), std::move(variables)};
}
