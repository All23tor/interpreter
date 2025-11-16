#include "Interpreter.hpp"
#include <format>
#include <iostream>
#include <memory>
#include <print>
#include <type_traits>

template <>
struct std::formatter<Value> {
  constexpr auto parse(std::format_parse_context& ctx) {
    return ctx.begin();
  }
  auto format(const Value& val, std::format_context& ctx) const {
    return std::visit(
      [idx = val.v.index(), &ctx]<class T>(const T& arg) {
        if constexpr (std::is_same_v<T, std::string>)
          return std::format_to(ctx.out(), "{:?}", arg);
        else if constexpr (std::is_same_v<T, std::monostate>)
          return std::format_to(ctx.out(), "()");
        else if constexpr (std::is_same_v<T, Ptr>)
          return std::format_to(ctx.out(), "ptr -> {}", *arg.ptr);
        else if constexpr (std::is_same_v<T, float>)
          return std::format_to(ctx.out(), "{:#}", arg);
        else
          return std::format_to(ctx.out(), "{}", arg);
      },
      val.v
    );
  }
};

template <>
struct std::formatter<Expression> {
  constexpr auto parse(std::format_parse_context& ctx) {
    return ctx.begin();
  }
  auto format(const Expression& expr, std::format_context& ctx) const {
    return std::visit(
      [&ctx](const Value& arg) { return std::format_to(ctx.out(), "{}", arg); },
      expr.v
    );
  }
};

int main() {
  Context context;
  std::string expression;
  while (!std::cin.eof()) {
    std::print(std::clog, "> ");
    std::getline(std::cin, expression, ';');
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
