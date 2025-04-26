#include "Interpreter.hpp"
#include <iostream>

Context makeContext(const std::set<std::string>& variables) {
  Context context;
  for (const auto& name : variables) {
    while (true) {
      try {
        std::clog << '\t' << name << ": ";
        std::string value;
        std::cin >> value;
        context[name] = std::stof(value);
        break;
      } catch (...) {
        std::cerr << '\t' << "Wrong assignment\n";
      }
    }
  }
  return context;
}

int main() {
  std::clog << "Expression: ";
  std::string expression;
  while (expression.empty())
    std::getline(std::cin, expression);

  try {
    auto [tree, variables] = parseExpression(std::move(expression));
    Context context = makeContext(variables);
    std::visit(
        [](auto&& arg) {
          std::cout << std::boolalpha << arg << '\n';
        },
        tree->evaluate(context));
  } catch (...) {
    std::cerr << "Wrong syntax\n";
    return -1;
  }
}
