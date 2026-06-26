#pragma once

#include <bitset>
#include <functional>
#include <optional>
#include <string>

// === Positive Tests ===

// Test Suite 1: std::bitset
using Bitset = std::bitset<64>;
Bitset makeBitset(uint64_t n) { return std::bitset<64>{n}; }

// Test Suite 2: std::optional<std::string>
using Optionalstr = std::optional<std::string>;
using hashoptstr = std::hash<Optionalstr>;
using eqoptstr = std::equal_to<Optionalstr>;

// Test Suite 3: member operator==
struct A {
  int value;
  std::string comment;

  bool operator==(const A &rhs) const { return value == rhs.value; }
};
template <>
struct std::hash<A> {
  size_t operator()(const A &x) const { return std::hash<int>{}(x.value); }
};

// Test Suite 4: global operator==
struct B {
  int value;
  std::string comment;
};
bool operator==(const B &lhs, const B &rhs) { return lhs.value == rhs.value; }
template <>
struct std::hash<B> {
  size_t operator()(const B &x) const { return std::hash<int>{}(x.value); }
};

// Test Suite 5: C string and std::equal_to
struct C {
  int value, len;
  char *comment;

  C(int value, std::string comment) : value(value), len(comment.size()) {
    this->comment = new char[len + 1];
    for (int i = 0; i <= comment.size(); ++i)
      this->comment[i] = comment[i];
  }
};
template <>
struct std::hash<C> {
  size_t operator()(const C &x) const { return std::hash<int>{}(x.value); }
};
template <>
struct std::equal_to<C> {
  bool operator()(const C &lhs, const C &rhs) const {
    return lhs.value == rhs.value;
  }
};

// === Negative Tests ===

// Test Suite 1: no hash
struct D {
  int x, y;
};

// Test Suite 2: no operator== or std::equal_to
struct E {
  int x, y;
};
template <>
struct std::hash<E> {
  size_t operator()(const E &x) const { return std::hash<int>{}(x.y); }
};

// Test Suite 3: incorrect hash specialization
struct F {
  int value;
  std::string comment;

  bool operator==(const F &rhs) const { return value == rhs.value; }
};

template <>
struct std::hash<F> {};
