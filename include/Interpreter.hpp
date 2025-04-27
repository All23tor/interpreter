#include <map>
#include <memory>
#include <set>
#include <string>
#include <variant>

using Value = std::variant<bool, int, float, std::string>;
using Context = std::map<std::string, Value>;

struct Node {
  virtual ~Node() = default;
  virtual Value evaluate(const Context&) const = 0;
};
using NodePtr = std::unique_ptr<Node>;

struct ParseResult {
  NodePtr tree;
  std::set<std::string> usedVariables;
};

ParseResult parseExpression(std::string);
