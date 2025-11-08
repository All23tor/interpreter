#include <map>
#include <memory>
#include <variant>

struct Value {
  std::variant<bool, int, float, std::string> v;
};
using Context = std::map<std::string, Value>;

Value operator||(const Value&, const Value&);
Value operator&&(const Value&, const Value&);
Value operator>=(const Value&, const Value&);
Value operator<=(const Value&, const Value&);
Value operator>(const Value&, const Value&);
Value operator<(const Value&, const Value&);
Value operator==(const Value&, const Value&);
Value operator!=(const Value&, const Value&);
Value operator+(const Value&, const Value&);
Value operator-(const Value&, const Value&);
Value operator*(const Value&, const Value&);
Value operator/(const Value&, const Value&);
Value operator%(const Value&, const Value&);

struct Node {
  virtual ~Node() = default;
  virtual Value evaluate(const Context&) const = 0;
};

Value parse_value(std::string_view);
std::unique_ptr<Node> parse_expression(std::string_view);
