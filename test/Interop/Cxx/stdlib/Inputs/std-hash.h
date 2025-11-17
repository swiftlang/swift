#pragma once

#include <optional>
#include <string>

using Optionalstr = std::optional<std::string>;
using hashoptstr = std::hash<Optionalstr>;
using eqoptstr = std::equal_to<Optionalstr>;

struct A {
  int value;
  std::string comment;

  bool operator==(const A &rhs) const { return value == rhs.value; }
};

template <>
struct std::hash<A> {
  size_t operator()(const A &x) const { return std::hash<int>{}(x.value); }
};

struct B {
  int value, len;
  char *comment;

  B(int value, std::string comment) : value(value), len(comment.size()) {
    this->comment = new char[len + 1];
    for (int i = 0; i <= comment.size(); ++i)
      this->comment[i] = comment[i];
  }
};

template <>
struct std::hash<B> {
  size_t operator()(const B &x) const { return std::hash<int>{}(x.value); }
};

template <>
struct std::equal_to<B> {
  bool operator()(const B &lhs, const B &rhs) const {
    return lhs.value == rhs.value;
  }
};

struct C {
  int x, y;
};

struct D {
  int x, y;
};

template <class T>
struct hash;

template <>
struct hash<D> {
  size_t operator()(const D &x) const { return std::hash<int>{}(x.y); }
};
