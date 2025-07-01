#include "Interpreter.hpp"
#include <iomanip>
#include <iostream>
#include <stdexcept>

Value force_evaluate(auto&& tree) {
  Context context;

  while (true)
    try {
      return tree->evaluate(context);
    } catch (std::string& var_name) {
      std::cout << "$" << var_name << ": ";
      std::string value;
      std::cin >> value;
      context[var_name] = parse_value(value);
    }
}

void interpret(std::string&& expression) {
  NodePtr tree;

  try {
    tree = parseExpression(std::move(expression));
  } catch (std::invalid_argument& ia) {
    std::cerr << "Unrecognized value\n";
    return;
  }

  try {
    std::visit(
        [](auto&& arg) {
          using T = std::remove_cvref_t<decltype(arg)>;
          if constexpr (std::is_same_v<std::string, T>)
            std::cout << '\t' << std::quoted(arg) << '\n';
          else
            std::cout << '\t' << std::boolalpha << arg << '\n';
        },
        force_evaluate(tree));
  } catch (std::bad_variant_access& bva) {
    std::cerr << "Unsuported operator\n";
    return;
  }
}

int main() {
  while (!std::cin.eof()) {
    std::string expression;
    std::clog << "> ";
    std::getline(std::cin, expression);
    if (expression.empty())
      continue;
    interpret(std::move(expression));
  }
  std::cout << '\n';
}
