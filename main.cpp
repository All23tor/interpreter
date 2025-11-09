#include "Interpreter.hpp"
#include <format>
#include <iostream>
#include <memory>
#include <print>

template <>
struct std::formatter<Value> {
  static constexpr std::array value_names = {
    "unit", "bool", "int", "float", "string", "ref"
  };
  constexpr auto parse(std::format_parse_context& ctx) {
    return ctx.begin();
  }
  auto format(const Value& v, std::format_context& ctx) const {
    return std::visit(
      [idx = v.v.index(), &ctx]<class T>(const T& arg) {
        if constexpr (std::is_same_v<T, std::string>)
          return std::format_to(ctx.out(), "{}: {:?}", value_names[idx], arg);
        else if constexpr (std::is_same_v<T, std::monostate>)
          return std::format_to(ctx.out(), "{}: ()", value_names[idx]);
        else if constexpr (std::is_same_v<T, Ref>)
          return std::format_to(
            ctx.out(), "{}: {}", value_names[idx], arg.ref->first
          );
        else
          return std::format_to(ctx.out(), "{}: {}", value_names[idx], arg);
      },
      v.v
    );
  }
};

int main() {
  Context context;
  std::string expression;
  while (!std::cin.eof()) {
    std::print(std::clog, "> ");
    std::getline(std::cin, expression);
    if (expression.empty())
      continue;
    try {
      std::unique_ptr<Node> tree = parse_expression(expression);
      std::println("  {}", tree->evaluate(context));
    } catch (std::exception& e) {
      std::println(std::cerr, "{}", e.what());
    }
  }
  std::println("^D Quitting");
}
