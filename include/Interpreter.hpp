#include <map>
#include <memory>
#include <utility>
#include <variant>

struct Value;
struct Ptr {
  Value* ptr;
};
struct Value {
  std::variant<std::monostate, bool, int, float, std::string, Ptr> v;
  template <class T>
  Value(T&& arg) : v(std::forward<T>(arg)) {}
};
using Context = std::map<std::string, Value>;
struct LValue {
  Value* ref;
  operator Value&() & {
    return *ref;
  }
  operator const Value&() const& {
    return *ref;
  }
};
struct RValue {
  Value val;
  operator Value&() & {
    return val;
  }
  operator const Value&() const& {
    return val;
  }
};
struct Expression {
  std::variant<RValue, LValue> v;
  template <class T>
  Expression(T&& arg) : v(std::forward<T>(arg)) {}
};

struct Node {
  virtual ~Node() = default;
  virtual Expression evaluate(Context&) const = 0;
};

Value parse_value(std::string_view);
std::unique_ptr<Node> parse_expression(std::string_view);
