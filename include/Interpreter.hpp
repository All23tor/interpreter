#include <map>
#include <memory>
#include <variant>

struct Value;
using Context = std::map<std::string, Value>;
struct Ref {
  Context::iterator ref;
};
struct Value {
  std::variant<std::monostate, bool, int, float, std::string, Ref> v;
};

struct Node {
  virtual ~Node() = default;
  virtual Value evaluate(Context&) const = 0;
};

Value parse_value(std::string_view);
std::unique_ptr<Node> parse_expression(std::string_view);
