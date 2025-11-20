#include "Interpreter.hpp"
#include <algorithm>
#include <array>
#include <charconv>
#include <format>
#include <functional>
#include <ranges>
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
struct Assign;
struct DerefOf;
struct RefOf;
using name_types = std::tuple<
  NameType<int, "int">,
  NameType<float, "float">,
  NameType<bool, "bool">,
  NameType<std::string, "string">,
  NameType<std::monostate, "unit">,
  NameType<Ptr, "ptr">,
  NameType<RValue, "rvalue">,
  NameType<LValue, "lvalue">,
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
  NameType<std::negate<>, "-">,
  NameType<std::logical_not<>, "!">,
  NameType<Assign, "=">,
  NameType<RefOf, "&">,
  NameType<DerefOf, "*">>;

template <class T, std::size_t I = 0>
consteval std::string_view name_type() {
  using Nt = std::tuple_element_t<I, name_types>;
  if constexpr (std::is_same_v<T, typename Nt::T>)
    return Nt::N;
  else
    return name_type<T, I + 1>();
}

template <class F>
static constexpr auto adapt_binary = [](const Value& a, const Value& b) {
  return std::visit(
    []<class A, class B>(const A& a, const B& b) -> RValue {
      if constexpr (requires { Value{F{}(a, b)}; })
        return RValue{F{}(a, b)};
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
    a.v,
    b.v
  );
};
template <class F>
static constexpr auto adapt_unary = [](const Value& val) {
  return std::visit(
    []<class A>(const A& a) -> RValue {
      if constexpr (requires { Value{F{}(a)}; })
        return RValue{F{}(a)};
      else
        throw std::runtime_error(
          std::format(
            "Operator '{}' does not support type '{}'",
            name_type<F>(),
            name_type<A>()
          )
        );
    },
    val.v
  );
};
} // namespace

static auto operator||(const Value& lhs, const Value& rhs) {
  return adapt_binary<std::logical_or<>>(lhs, rhs);
}
static auto operator&&(const Value& lhs, const Value& rhs) {
  return adapt_binary<std::logical_and<>>(lhs, rhs);
}
static auto operator>=(const Value& lhs, const Value& rhs) {
  return adapt_binary<std::greater_equal<>>(lhs, rhs);
}
static auto operator<=(const Value& lhs, const Value& rhs) {
  return adapt_binary<std::less_equal<>>(lhs, rhs);
}
static auto operator>(const Value& lhs, const Value& rhs) {
  return adapt_binary<std::greater<>>(lhs, rhs);
}
static auto operator<(const Value& lhs, const Value& rhs) {
  return adapt_binary<std::less<>>(lhs, rhs);
}
static auto operator==(const Value& lhs, const Value& rhs) {
  return adapt_binary<std::equal_to<>>(lhs, rhs);
}
static auto operator!=(const Value& lhs, const Value& rhs) {
  return adapt_binary<std::not_equal_to<>>(lhs, rhs);
}
static auto operator+(const Value& lhs, const Value& rhs) {
  return adapt_binary<std::plus<>>(lhs, rhs);
}
static auto operator-(const Value& lhs, const Value& rhs) {
  return adapt_binary<std::minus<>>(lhs, rhs);
}
static auto operator*(const Value& lhs, const Value& rhs) {
  return adapt_binary<std::multiplies<>>(lhs, rhs);
}
static auto operator/(const Value& lhs, const Value& rhs) {
  return adapt_binary<std::divides<>>(lhs, rhs);
}
static auto operator%(const Value& lhs, const Value& rhs) {
  return adapt_binary<std::modulus<>>(lhs, rhs);
}
static auto operator-(const Value& val) {
  return adapt_unary<std::negate<>>(val);
}
static auto operator!(const Value& val) {
  return adapt_unary<std::logical_not<>>(val);
}

namespace {
struct Assign {
  LValue operator()(LValue ref, Value val) {
    *ref.ref = val;
    return ref;
  }
};
struct RefOf {
  RValue operator()(LValue ref) {
    return RValue{Ptr{ref.ref}};
  }
};
struct DerefOf {
  LValue operator()(Value value) {
    return std::visit(
      []<class T>(const T& arg) -> LValue {
        if constexpr (std::is_same_v<T, Ptr>)
          return LValue{arg.ptr};
        else
          throw std::runtime_error(
            std::format("Operator * does not support type '{}'", name_type<T>())
          );
      },
      value.v
    );
  }
};

using sv = std::string_view;
constexpr auto is_alnum = [](unsigned char c) { return std::isalnum(c); };
constexpr auto is_digit = [](unsigned char c) { return std::isdigit(c); };
constexpr auto is_space = [](unsigned char c) { return std::isspace(c); };
sv trim(sv str) {
  auto b = std::ranges::find_if_not(str, is_space);
  auto e = std::ranges::find_last_if_not(str, is_space).begin();
  return str.substr(b - str.begin(), (e - b) + 1);
}
bool is_variable_name(sv name) {
  return !name.empty() && !is_digit(name.front()) &&
    std::ranges::all_of(name, [](unsigned char c) {
      return is_alnum(c) || c == '_';
    });
}

struct InvalidExpression : std::runtime_error {
  InvalidExpression(const std::string& what) : std::runtime_error(what) {}
};
struct EmptyExpression : InvalidExpression {
  EmptyExpression() :
    InvalidExpression("Expected an expression, found nothing instead") {};
};
struct InvalidVariable : InvalidExpression {
  InvalidVariable(sv _name) :
    InvalidExpression(std::format("'{}' is not a valid variable name", _name)) {
  }
};
struct InvalidOperator : InvalidExpression {
  InvalidOperator(sv expr, sv op_name, sv reason) :
    InvalidExpression(
      std::format(
        "Failed to parse operand for operator '{}' in expression '{}'\n"
        "Reason: {}",
        op_name,
        expr,
        reason
      )
    ) {}
};

using NodePtr = std::unique_ptr<Node>;
struct LiteralNode final : public Node {
  const Value value;

  LiteralNode(Value _value) : value{std::move(_value)} {}
  virtual ~LiteralNode() override = default;
  virtual Expression evaluate(Context&) const override {
    return RValue{value};
  }
};

struct VariableNode final : public Node {
  const std::string name;

  VariableNode(sv _name) :
    name([](auto _name) {
      if (!is_variable_name(_name))
        throw InvalidVariable(_name);
      return _name;
    }(_name)) {}
  virtual ~VariableNode() override = default;
  virtual Expression evaluate(Context& ctx) const override {
    return LValue{&ctx.at(name)};
  }
};

template <class F>
struct BinaryOperationNode final : public Node {
  static constexpr sv op_name = name_type<F>();
  NodePtr left;
  NodePtr right;

  BinaryOperationNode(sv expr, std::size_t pos) try :
    left(parse_expression(expr.substr(0, pos))),
    right(parse_expression(expr.substr(pos + op_name.size()))) {
  } catch (const InvalidExpression& e) {
    throw InvalidOperator(expr, op_name, e.what());
  }
  virtual ~BinaryOperationNode() override = default;
  virtual Expression evaluate(Context& ctx) const override {
    auto lhs = left->evaluate(ctx).v;
    auto rhs = right->evaluate(ctx).v;

    return std::visit(
      []<class A, class B>(const A& a, const B& b) -> Expression {
        if constexpr (requires { F{}(a, b); })
          return F{}(a, b);
        else
          throw std::runtime_error(
            std::format(
              "Operator '{}' does not support categories '{}' and '{}'",
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
  static constexpr sv op_name = "let ";
  const std::string name;

  explicit LetNode(sv _name) :
    name([](auto _name) {
      if (!is_variable_name(_name))
        throw InvalidVariable(_name);
      return _name;
    }(_name.substr(op_name.size()))) {}
  virtual ~LetNode() override = default;
  virtual Expression evaluate(Context& ctx) const override {
    return LValue{&ctx.insert(name)};
  }
};

template <class F>
struct UnaryOperationNode final : public Node {
  static constexpr sv op_name = name_type<F>();
  NodePtr expr;

  UnaryOperationNode(sv _expr) try :
    expr(parse_expression(_expr.substr(op_name.size()))) {
  } catch (const InvalidExpression& e) {
    throw InvalidOperator(_expr, op_name, e.what());
  }
  virtual ~UnaryOperationNode() override = default;
  virtual Expression evaluate(Context& ctx) const override {
    auto inner = expr->evaluate(ctx).v;

    return std::visit(
      []<class A>(const A& a) -> Expression {
        if constexpr (requires { F{}(a); })
          return F{}(a);
        else
          throw std::runtime_error(
            std::format(
              "Operator '{}' does not support category '{}'",
              name_type<F>(),
              name_type<A>()
            )
          );
      },
      inner
    );
  }
};

using NodeFactory = NodePtr (*)(sv, std::size_t);

struct OperatorInfo {
  sv name;
  NodeFactory factory;
  bool left_associative;
  bool unary;
  int precedence;
};

template <class F, bool A, int P>
static constexpr OperatorInfo unary_info = {
  .name = UnaryOperationNode<F>::op_name,
  .factory = [](sv expr, std::size_t) -> NodePtr {
    return std::make_unique<UnaryOperationNode<F>>(expr);
  },
  .left_associative = A,
  .unary = true,
  .precedence = P,
};

template <class F, bool A, int P>
static constexpr OperatorInfo binary_info = {
  .name = BinaryOperationNode<F>::op_name,
  .factory = [](sv expr, std::size_t pos) -> NodePtr {
    return std::make_unique<BinaryOperationNode<F>>(expr, pos);
  },
  .left_associative = A,
  .unary = false,
  .precedence = P,
};

static constexpr OperatorInfo let_info = {
  .name = LetNode::op_name,
  .factory = [](sv expr, std::size_t) -> NodePtr {
    return std::make_unique<LetNode>(expr);
  },
  .left_associative = true,
  .unary = true,
  .precedence = 0,
};

static constexpr std::array operators = {
  binary_info<Assign, false, 16>,
  binary_info<std::logical_or<>, true, 15>,
  binary_info<std::logical_and<>, true, 14>,
  binary_info<std::equal_to<>, true, 10>,
  binary_info<std::not_equal_to<>, true, 10>,
  binary_info<std::greater_equal<>, true, 9>,
  binary_info<std::less_equal<>, true, 9>,
  binary_info<std::greater<>, true, 9>,
  binary_info<std::less<>, true, 9>,
  binary_info<std::plus<>, true, 6>,
  binary_info<std::minus<>, true, 6>,
  binary_info<std::multiplies<>, true, 5>,
  binary_info<std::divides<>, true, 5>,
  binary_info<std::modulus<>, true, 5>,
  unary_info<std::negate<>, false, 3>,
  unary_info<std::logical_not<>, false, 3>,
  unary_info<RefOf, false, 3>,
  unary_info<DerefOf, false, 3>,
  let_info,
};
static constexpr OperatorInfo NullInfo{
  .precedence = -1,
};

bool is_unary_operator(sv expr, std::size_t operator_pos) {
  auto left = trim(expr.substr(0, operator_pos));
  return left.empty() || (left.back() != ')' && !is_alnum(left.back()));
}

NodePtr try_parse_operator(sv expr) {
  int parenthesis = 0;

  auto best_op = NullInfo;
  auto best_pos = sv::npos;
  for (auto [pos, c] : std::views::enumerate(expr)) {
    auto subx = expr.substr(pos);
    bool is_unary_pos = is_unary_operator(expr, pos);
    if (parenthesis == 0) {
      auto matches = std::views::filter(operators, [=](auto&& oi) {
        return subx.starts_with(oi.name) && oi.unary == is_unary_pos;
      });
      auto best_match = std::ranges::max_element(matches, {}, [](auto&& oi) {
        return oi.name.size();
      });
      if (best_match != matches.end()) {
        auto oi = *best_match;
        if (oi.precedence > best_op.precedence ||
            oi.precedence == best_op.precedence && oi.left_associative) {
          best_op = oi;
          best_pos = pos;
        }
      }
    }

    if (c == '(')
      ++parenthesis;
    else if (c == ')')
      --parenthesis;
  }
  if (best_pos != sv::npos)
    return best_op.factory(expr, best_pos);
  return nullptr;
}

bool encapsulated(sv expr) {
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

std::unique_ptr<LiteralNode> try_parse_value(sv expr) {
  if (expr == "()")
    return std::make_unique<LiteralNode>(Value{std::monostate{}});
  if (expr == "true")
    return std::make_unique<LiteralNode>(Value{true});
  if (expr == "false")
    return std::make_unique<LiteralNode>(Value{false});
  if (expr.starts_with('"') && expr.find('"', 1) + 1 == expr.size())
    return std::make_unique<LiteralNode>(
      Value{std::string(expr.substr(1, expr.size() - 2))}
    );
  {
    int i{};
    const auto [ptr, ec] =
      std::from_chars(expr.data(), expr.data() + expr.size(), i);
    if (ec == std::errc() && ptr == expr.data() + expr.size())
      return std::make_unique<LiteralNode>(Value{i});
  }
  {
    float f{};
    const auto [ptr, ec] =
      std::from_chars(expr.data(), expr.data() + expr.size(), f);
    if (ec == std::errc() && ptr == expr.data() + expr.size())
      return std::make_unique<LiteralNode>(Value{f});
  }
  return nullptr;
}
} // namespace

void Context::push_frame() {
  frames_stack.emplace_back();
}
void Context::pop_frame() {
  frames_stack.pop_back();
}
Value& Context::at(const std::string& name) {
  for (auto&& frame : std::views::reverse(frames_stack))
    if (auto it = frame.find(name); it != frame.end())
      return it->second;
  throw std::runtime_error(
    std::format("'{}' was not declared in this scope", name)
  );
}
Value& Context::insert(const std::string& name) {
  auto [it, inserted] =
    frames_stack.back().insert({name, Value{std::monostate{}}});
  if (!inserted)
    throw std::runtime_error(
      std::format("Variable '{}' already declared in this scope", name)
    );
  return it->second;
}

NodePtr parse_expression(sv expr) {
  expr = trim(expr);
  while (expr.size() > 2 && encapsulated(expr))
    expr = trim(expr.substr(1, expr.size() - 2));

  if (expr.empty())
    throw EmptyExpression();
  if (auto opt_value = try_parse_value(expr))
    return opt_value;
  if (auto opt_op = try_parse_operator(expr))
    return opt_op;
  return std::make_unique<VariableNode>(expr);
}
