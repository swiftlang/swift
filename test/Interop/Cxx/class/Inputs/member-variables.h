#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_MEMBER_VARIABLES_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_MEMBER_VARIABLES_H

#include <cstddef>
#include <type_traits>
#include <optional>

#if defined(_MSC_VER)
#define NO_UNIQUE_ADDRESS [[msvc::no_unique_address]]
#else
#define NO_UNIQUE_ADDRESS [[no_unique_address]]
#endif

class MyClass {
public:
  const int const_member = 23;
};

struct Empty {
  using type = int;
  int getNum() const { return 42; }
};

struct HasZeroSizedField {
  int a;
  NO_UNIQUE_ADDRESS Empty b;
  short c;
  NO_UNIQUE_ADDRESS Empty d;
  int* e;
  NO_UNIQUE_ADDRESS Empty f;

  int get_a() const { return a; }
  short get_c() const { return c; }
  void set_c(short c) { this->c = c; }
};

struct ReuseOptionalFieldPadding {
  NO_UNIQUE_ADDRESS std::optional<int> a = {2};
  char c;
  char get_c() const { return c; }
  void set_c(char c) { this->c = c; }
  int offset() const { return offsetof(ReuseOptionalFieldPadding, c); }
  std::optional<int> getOptional() { return a; }
};

using OptInt = std::optional<int>;

struct ReuseOptionalFieldPaddingWithTypedef {
  NO_UNIQUE_ADDRESS OptInt a;
  char c;
  char get_c() const { return c; }
  void set_c(char c) { this->c = c; }
  int offset() const { return offsetof(ReuseOptionalFieldPadding, c); }
};

// Using a mix of private and public fields prevents this class from being
// standard-layout, which is necessary to allow clang to reuse its padding.
template<typename T>
struct NonStandardLayoutClass {
private:
  T x;
public:
  char pad_me;
};
static_assert(std::is_standard_layout_v<NonStandardLayoutClass<int>> == false);

struct ReuseNonStandardLayoutFieldPadding {
  NO_UNIQUE_ADDRESS NonStandardLayoutClass<int> a;
  char c;
  char get_c() const { return c; }
  void set_c(char c) { this->c = c; }
  // C-style implementation of offsetof() to avoid non-standard-layout warning
  int offset() const { return (char *) &this->c - (char *) this; }
};

template<typename T>
struct ReuseDependentFieldPadding {
  NO_UNIQUE_ADDRESS struct { private: T x; public: char pad_me; } a;
  char c;
  char get_c() const { return c; }
  void set_c(char c) { this->c = c; }
  // C-style implementation of offsetof() to avoid non-standard-layout warning
  int offset() const { return (char *) &this->c - (char *) this; }
};

typedef ReuseDependentFieldPadding<int> ReuseDependentFieldPaddingInt;

inline int takesZeroSizedInCpp(HasZeroSizedField x) {
  return x.a;
}

/// Not imported into Swift.
struct EmptyNotImported {
  char : 0;

  EmptyNotImported() = default;
  EmptyNotImported(const EmptyNotImported &) = delete;
  EmptyNotImported(EmptyNotImported &&) = delete;
};

struct LastFieldNoUniqueAddress {
  char c;
  NO_UNIQUE_ADDRESS EmptyNotImported p0;

  LastFieldNoUniqueAddress(const LastFieldNoUniqueAddress &other) {}
  LastFieldNoUniqueAddress(LastFieldNoUniqueAddress &&other) {}
};

#endif
