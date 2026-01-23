#include <string>
#include <variant>

using StdVariantIntOrString = std::variant<int, std::string>;
inline StdVariantIntOrString getStdVariantInt() { return {123}; }
inline StdVariantIntOrString getStdVariantString() { return {"abc"}; }

struct HasDeletedCopyCtor {
  int value;
  HasDeletedCopyCtor(int value) : value(value) {}
  HasDeletedCopyCtor(const HasDeletedCopyCtor &other) = delete;
  HasDeletedCopyCtor(HasDeletedCopyCtor &&other) = default;
};

using StdVariantIntOrNonCopyable = std::variant<HasDeletedCopyCtor, int>;
inline StdVariantIntOrNonCopyable getStdVariantNonCopyable() { return HasDeletedCopyCtor(123); }
