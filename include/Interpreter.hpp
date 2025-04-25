#include <map>
#include <memory>
#include <set>
#include <string>

using Context = std::map<std::string, float>;

struct Node {
  virtual ~Node() = default;
  virtual float evaluate(const Context&) const = 0;
};

struct ParseResult {
  std::unique_ptr<Node> tree;
  std::set<std::string> usedVariables;
};

ParseResult parseExpression(const std::string&);
