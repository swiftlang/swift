#ifndef DEFAULTED_COMPARISONS_H
#define DEFAULTED_COMPARISONS_H

#include <compare>

struct SingleField {
  int value;

  SingleField(int v) : value(v) {}
  SingleField(const SingleField &) = default;

  bool operator==(const SingleField &) const = default;
};

struct MultipleFields {
  int x;
  int y;

  MultipleFields(int x, int y) : x(x), y(y) {}
  MultipleFields(const MultipleFields &) = default;

  bool operator==(const MultipleFields &) const = default;
};

struct Inner {
  int value;

  Inner(int v) : value(v) {}
  Inner(const Inner &) = default;

  bool operator==(const Inner &) const = default;
};

struct Outer {
  Inner inner;

  Outer(int v) : inner(v) {}
  Outer(const Outer &) = default;

  bool operator==(const Outer &) const = default;
};

struct OuterExplicit {
  Inner inner;

  OuterExplicit(int v) : inner(v) {}
  OuterExplicit(const OuterExplicit &) = default;

  bool operator==(const OuterExplicit &other) const {
    return inner == other.inner;
  }
};

struct FriendEq {
  int value;

  FriendEq(int v) : value(v) {}
  FriendEq(const FriendEq &) = default;

  friend bool operator==(const FriendEq &, const FriendEq &) = default;
};

struct WithNotEqual {
  int value;

  WithNotEqual(int v) : value(v) {}
  WithNotEqual(const WithNotEqual &) = default;

  bool operator==(const WithNotEqual &) const = default;
  bool operator!=(const WithNotEqual &) const = default;
};

struct Spaceship {
  int value;

  Spaceship(int v) : value(v) {}
  Spaceship(const Spaceship &) = default;

  auto operator<=>(const Spaceship &) const = default;
};

struct Base {
  int x;

  Base(int x) : x(x) {}
  Base(const Base &) = default;

  bool operator==(const Base &) const = default;
};

struct Derived : Base {
  int y;

  Derived(int x, int y) : Base(x), y(y) {}
  Derived(const Derived &) = default;

  bool operator==(const Derived &) const = default;
};

template <typename T>
struct Wrapper {
  T value;

  Wrapper(T v) : value(v) {}
  Wrapper(const Wrapper &) = default;

  bool operator==(const Wrapper &) const = default;
};

using IntWrapper = Wrapper<int>;

#endif
