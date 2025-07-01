#include "Interpreter.hpp"
#include <iomanip>
#include <iostream>
#include <stdexcept>

int main() {
  std::string expression;
  while (expression.empty()) {
    std::clog << "> ";
    if (!std::getline(std::cin, expression)) {
      std::cout << '\n';
      return 0;
    }
  }

  try {
    auto tree = parseExpression(std::move(expression));
    Context context;

    Value ans;
    while (true)
      try {
        ans = tree->evaluate(context);
        break;
      } catch (std::string& var_name) {
        std::cout << "\t$" << var_name << ": ";
        std::string value;
        std::cin >> value;
        context[var_name] = parse_value(value);
      }

    std::visit(
        [](auto&& arg) {
          using T = std::remove_cvref_t<decltype(arg)>;
          if constexpr (std::is_same_v<std::string, T>)
            std::cout << '\t' << std::quoted(arg) << '\n';
          else
            std::cout << '\t' << std::boolalpha << arg << '\n';
        },
        ans);
  } catch (std::bad_variant_access& bva) {
    std::cerr << "Unsuported operator\n";
    return -1;
  } catch (std::invalid_argument& ia) {
    std::cerr << "Unrecognized value\n";
    return -1;
  }
}
