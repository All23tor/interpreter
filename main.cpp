#include "Interpreter.hpp"
#include <format>
#include <iostream>
#include <memory>
#include <print>

static constexpr std::array value_names = {
  "unit", "bool", "int", "float", "string"
};
template <>
struct std::formatter<Value> {
  constexpr auto parse(std::format_parse_context& ctx) {
    return ctx.begin();
  }
  auto format(const Value& v, std::format_context& ctx) const {
    return std::visit(
      [idx = v.v.index(), &ctx]<class T>(const T& arg) {
        if constexpr (std::is_same_v<T, std::string>)
          return std::format_to(ctx.out(), "{} '{}'", value_names[idx], arg);
        else if constexpr (std::is_same_v<T, std::monostate>)
          return std::format_to(ctx.out(), "{}", value_names[idx]);
        else
          return std::format_to(ctx.out(), "{} {}", value_names[idx], arg);
      },
      v.v
    );
  }
};

Value force_parse(const std::string var_name) {
  std::string value;

  while (true)
    try {
      return parse_value(value);
    } catch (std::invalid_argument& ia) {
      std::print("{}: ", var_name);
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
    std::println(std::cerr, "{}", ia.what());
    return;
  }

  try {
    std::println("  {}", force_evaluate(tree));
  } catch (std::exception& e) {
    std::println(std::cerr, "{}", e.what());
  }
}

int main() {
  std::string expression;
  while (!std::cin.eof()) {
    std::print(std::clog, "> ");
    std::getline(std::cin, expression);
    if (expression.empty())
      continue;
    interpret(expression);
  }
  std::println();
}
