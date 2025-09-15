#include "Interpreter.hpp"
#include <algorithm>
#include <array>
#include <functional>
#include <string_view>
#include <utility>

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
    } catch (std::out_of_range& oor) {
      throw name;
    }
  }
};

template <class T1, class T2, class F>
struct OperationException;

template <class Func>
struct OperationNode final : public Node {
  static constexpr auto operation = [](auto&& a, auto&& b) -> Value {
    if constexpr (requires { Func{}(a, b); })
      return Func{}(a, b);
    else
      throw OperationException<
        std::remove_cvref_t<decltype(a)>,
        std::remove_cvref_t<decltype(b)>,
        Func>{};
  };
  const SyntaxTree left;
  const SyntaxTree right;

  OperationNode(SyntaxTree&& _left, SyntaxTree&& _right) :
    left(std::move(_left)),
    right(std::move(_right)) {}
  virtual ~OperationNode() override final = default;
  virtual Value evaluate(const Context& context) const override final {
    auto lhs = left->evaluate(context);
    auto rhs = right->evaluate(context);
    return std::visit(operation, std::move(lhs), std::move(rhs));
  }
};

using NodeFactory = SyntaxTree (*)(SyntaxTree&&, SyntaxTree&&);
template <typename Func>
constexpr NodeFactory op_factory =
  [](SyntaxTree&& left, SyntaxTree&& right) -> SyntaxTree {
  return std::make_unique<OperationNode<Func>>(
    std::move(left), std::move(right)
  );
};

struct OperationInfo {
  std::string_view name;
  NodeFactory factory;
};

template <auto N>
struct string_literal {
  constexpr string_literal(const char (&str)[N]) {
    std::copy_n(str, N, value);
  }
  char value[N];
};

template <class T, string_literal S>
struct type_name {
  using type = T;
  static constexpr std::string_view value = S.value;
};

using OPERATORS = std::tuple<
  type_name<std::logical_or<>, "||">,
  type_name<std::logical_and<>, "&&">,
  type_name<std::greater_equal<>, ">=">,
  type_name<std::less_equal<>, "<=">,
  type_name<std::greater<>, ">">,
  type_name<std::less<>, "<">,
  type_name<std::equal_to<>, "==">,
  type_name<std::not_equal_to<>, "!=">,
  type_name<std::plus<>, "+">,
  type_name<std::minus<>, "-">,
  type_name<std::multiplies<>, "*">,
  type_name<std::divides<>, "/">,
  type_name<std::modulus<>, "%">>;

template <class T, class... Ts>
struct find_literal;
template <class T>
struct find_literal<T> {};
template <class T, class H, class... Ts>
requires std::same_as<T, typename H::type>
struct find_literal<T, H, Ts...> {
  static constexpr std::string_view value = H::value;
};
template <class T, class H, class... Ts>
struct find_literal<T, H, Ts...> {
  static constexpr std::string_view value = find_literal<T, Ts...>::value;
};
template <class T, class... Ts>
struct find_literal<T, std::tuple<Ts...>> {
  static constexpr auto value = find_literal<T, Ts...>::value;
};

static constexpr std::array<OperationInfo, 13> operations = []() {
  static constexpr auto nth_op_info =
    []<std::size_t Idx = 0>(this auto nth_op_info, int idx) constexpr {
      using T = std::tuple_element_t<Idx, OPERATORS>;
      if (Idx == idx)
        return OperationInfo{T::value, op_factory<typename T::type>};
      else if constexpr (Idx + 1 != std::tuple_size_v<OPERATORS>)
        return nth_op_info.template operator()<Idx + 1>(idx);

      std::unreachable();
    };

  std::array<OperationInfo, 13> operations;
  for (int i = 0; i < std::tuple_size_v<OPERATORS>; i++)
    operations[i] = nth_op_info(i);
  return operations;
}();

template <class T1, class T2, class F>
struct OperationException : std::exception {
  template <const std::string_view&... views>
  struct concat_impl {
    static constexpr auto arr = [] {
      constexpr std::size_t len = (views.size() + ... + 0);
      std::array<char, len + 1> arr{};
      auto append = [i = 0, &arr](auto const& s) mutable {
        for (auto c : s)
          arr[i++] = c;
      };
      (append(views), ...);
      arr[len] = 0;
      return arr;
    }();

    static constexpr std::string_view value{arr.data(), arr.size() - 1};
  };
  template <const std::string_view&... views>
  static constexpr auto concat = concat_impl<views...>::value;

  static constexpr std::array<std::string_view, std::variant_size_v<Value>>
    VALUE_NAMES = {"bool", "int", "float", "string"};

  template <typename T, typename V>
  struct get_index;

  template <class T>
  struct tag {};
  template <typename T, typename... Ts>
  struct get_index<T, std::variant<Ts...>> {
    static constexpr std::size_t value =
      std::variant<tag<Ts>...>(tag<T>{}).index();
  };

  static constexpr std::string_view TYPES = "Types ";
  static constexpr std::size_t T1_IDX = get_index<T1, Value>::value;
  static constexpr std::string_view AND = " and ";
  static constexpr std::size_t T2_IDX = get_index<T2, Value>::value;
  static constexpr std::string_view UNSUPPORTED_FOR_OPERATOR =
    " unsupported for operator ";
  static constexpr std::string_view WHAT = concat<
    TYPES,
    VALUE_NAMES[T1_IDX],
    AND,
    VALUE_NAMES[T2_IDX],
    UNSUPPORTED_FOR_OPERATOR,
    find_literal<F, OPERATORS>::value>;

  virtual const char* what() const noexcept override {
    return WHAT.data();
  }

  virtual ~OperationException() override = default;
};

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

SyntaxTree make_tree(std::string_view expression) {
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

SyntaxTree parse_expression(std::string_view expression) {
  std::string formatted_expression{expression};
  std::erase(formatted_expression, ' ');
  return make_tree(formatted_expression);
}
