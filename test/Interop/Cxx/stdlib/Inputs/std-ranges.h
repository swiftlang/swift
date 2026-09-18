#include <ranges>

static auto dereferenceV = std::views::transform(
        [](auto &&x) -> decltype(auto) { return *x; });
