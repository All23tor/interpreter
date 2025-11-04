#include <map>
#include <memory>
#include <variant>

using Value = std::variant<bool, int, float, std::string>;
using Context = std::map<std::string, Value>;

struct Node {
  virtual ~Node() = default;
  virtual Value evaluate(const Context&) const = 0;
};

Value parse_value(std::string_view);
std::unique_ptr<Node> parse_expression(std::string_view);
