#include "Interpreter.hpp"
#include <iomanip>
#include <iostream>

Context makeContext(const std::set<std::string>& variables) {
  Context context;
  for (const auto& name : variables) {
    while (true) {
      try {
        std::clog << '\t' << name << ": ";
        std::string value;
        std::cin >> value;
        if (value.front() == '"' && value.length() > 1 && value.back() == '"') {
          value.pop_back();
          value.erase(0, 1);
          context[name] = std::move(value);
        } else if (value == "true") {
          context[name] = true;
        } else if (value == "false") {
          context[name] = false;
        } else if (value.contains('.'))
          context[name] = std::stof(value);
        else
          context[name] = std::stoi(value);
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
          using T = std::remove_cvref_t<decltype(arg)>;
          if constexpr (std::is_same_v<std::string, T>)
            std::cout << std::quoted(arg) << '\n';
          else
            std::cout << std::boolalpha << arg << '\n';
        },
        tree->evaluate(context));
  } catch (...) {
    std::cerr << "Wrong syntax\n";
    return -1;
  }
}
