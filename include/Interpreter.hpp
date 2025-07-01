#include <map>
#include <memory>
#include <string>
#include <variant>

using Value = std::variant<bool, int, float, std::string>;
using Context = std::map<std::string, Value>;

struct Node {
  virtual ~Node() = default;
  virtual Value evaluate(const Context&) const = 0;
};
using SyntaxTree = std::unique_ptr<Node>;

Value parse_value(std::string_view);
SyntaxTree parse_expression(std::string_view);
