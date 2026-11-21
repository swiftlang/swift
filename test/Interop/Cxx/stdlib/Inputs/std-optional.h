#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_OPTIONAL_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_OPTIONAL_H

#include <optional>
#include <string>
#include <vector>

using StdOptionalInt = std::optional<int>;
using StdOptionalBool = std::optional<bool>;
using StdOptionalString = std::optional<std::string>;
using StdOptionalOptionalInt = std::optional<std::optional<int>>;

struct HasConstexprCtor {
  int value;
  constexpr HasConstexprCtor(int value) : value(value) {}
  constexpr HasConstexprCtor(const HasConstexprCtor &other) = default;
  constexpr HasConstexprCtor(HasConstexprCtor &&other) = default;
};
using StdOptionalHasConstexprCtor = std::optional<HasConstexprCtor>;

struct HasDeletedCopyCtor {
  int value;
  HasDeletedCopyCtor(int value) : value(value) {}
  HasDeletedCopyCtor(const HasDeletedCopyCtor &other) = delete;
  HasDeletedCopyCtor(HasDeletedCopyCtor &&other) = default;
};
using StdOptionalHasDeletedCopyCtor = std::optional<HasDeletedCopyCtor>;
using StdOptionalMoveOnlyVector =
    std::optional<std::vector<HasDeletedCopyCtor>>;

struct MoveOnlyCounted {
  static inline int alive = 0;
  int value;
  MoveOnlyCounted(int value) : value(value) { ++alive; }
  MoveOnlyCounted(const MoveOnlyCounted &other) = delete;
  MoveOnlyCounted(MoveOnlyCounted &&other) : value(other.value) {
    other.value = -1;
    ++alive;
  }
  ~MoveOnlyCounted() { --alive; }
};
using StdOptionalMoveOnlyCounted = std::optional<MoveOnlyCounted>;

inline StdOptionalMoveOnlyCounted getNonNilOptionalMoveOnlyCounted() {
  return StdOptionalMoveOnlyCounted(MoveOnlyCounted(42));
}
inline int getMoveOnlyCountedAlive() { return MoveOnlyCounted::alive; }

// Copyable in C++, but imported as non-copyable.
struct __attribute__((swift_attr("~Copyable"))) AnnotatedNonCopyable {
  int value;
  AnnotatedNonCopyable(int value) : value(value) {}
};
using StdOptionalAnnotatedNonCopyable = std::optional<AnnotatedNonCopyable>;

using StdOptionalOptionalHasDeletedCopyCtor =
    std::optional<std::optional<HasDeletedCopyCtor>>;

inline std::vector<HasDeletedCopyCtor> makeMoveOnlyVector() {
  std::vector<HasDeletedCopyCtor> vec;
  vec.emplace_back(111);
  vec.emplace_back(222);
  return vec;
}

struct HasDeletedMoveCtor {
  int value;
  HasDeletedMoveCtor(int value) : value(value) {}
  HasDeletedMoveCtor(const HasDeletedMoveCtor &other) : value(other.value) {}
  HasDeletedMoveCtor(HasDeletedMoveCtor &&other) = delete;
};
using StdOptionalHasDeletedMoveCtor = std::optional<HasDeletedMoveCtor>;

inline StdOptionalInt getNonNilOptional() { return {123}; }

inline StdOptionalInt getNilOptional() { return {std::nullopt}; }

inline StdOptionalHasDeletedCopyCtor getNonNilOptionalHasDeletedCopyCtor() {
  return StdOptionalHasDeletedCopyCtor(HasDeletedCopyCtor(654));
}

inline StdOptionalMoveOnlyVector getNonNilOptionalMoveOnlyVector() {
  std::vector<HasDeletedCopyCtor> vec;
  vec.emplace_back(789);
  vec.emplace_back(987);
  return StdOptionalMoveOnlyVector(std::move(vec));
}

inline bool takesOptionalInt(std::optional<int> arg) { return (bool)arg; }
inline bool takesOptionalString(std::optional<std::string> arg) { return (bool)arg; }
inline bool takesOptionalHasDeletedCopyCtor(std::optional<HasDeletedCopyCtor> arg) { return (bool)arg; }

struct DerivedFromOptional : std::optional<std::string> {
  using std::optional<std::string>::optional;
};

struct ReturnsOptionalString {
  ReturnsOptionalString() {}
  std::optional<std::string> getStr() const { return "foo"; }
  DerivedFromOptional getStrDerived() const { return "foo"; }
};

// compiler crasher: optional of a type with user-defined conversion operator
//                   to an instantiated template type
template <typename T> struct Templated {};
struct ConvertsToTemplated { operator Templated<int>() const; };
std::optional<ConvertsToTemplated> returnsConvertsToTemplated();

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_OPTIONAL_H
