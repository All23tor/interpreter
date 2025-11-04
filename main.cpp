#include "Interpreter.hpp"
#include <iomanip>
#include <iostream>
#include <memory>

Value force_parse(const std::string var_name) {
  std::string value;

  while (true)
    try {
      return parse_value(value);
    } catch (std::invalid_argument& ia) {
      std::cout << "$" << var_name << ": ";
      std::getline(std::cin, value);
    }
}

Value force_evaluate(const std::unique_ptr<Node>& tree) {
  Context context;

  while (true)
    try {
      return tree->evaluate(context);
    } catch (const std::string& var_name) {
      context[var_name] = force_parse(var_name);
    }
}

void interpret(std::string_view expression) {
  std::unique_ptr<Node> tree;

  try {
    tree = parse_expression(expression);
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
      force_evaluate(tree)
    );
  } catch (std::exception& e) {
    std::cerr << e.what() << '\n';
  }
}

int main() {
  std::string expression;
  while (!std::cin.eof()) {
    std::clog << "> ";
    std::getline(std::cin, expression);
    if (expression.empty())
      continue;
    interpret(expression);
  }
  std::cout << '\n';
}
