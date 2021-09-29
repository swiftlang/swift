#ifndef TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_MOVE_ONLY_H
#define TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_MOVE_ONLY_H

#include <new>
#include <stdlib.h>
#include <utility>

#include "visibility.h"

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

struct __attribute__((swift_attr("import_as_ref"))) MoveOnly {
  MoveOnly() = default;
  MoveOnly(const MoveOnly &) = delete;
  MoveOnly(MoveOnly &&) = default;

  int test() const { return 42; }
  int testMutable() { return 42; }

  static MoveOnly *create() {
    return new (malloc(sizeof(MoveOnly))) MoveOnly();
  }
};

MoveOnly moveIntoResult(MoveOnly &x) { return std::move(x); }

struct __attribute__((swift_attr("import_as_ref"))) HasMoveOnlyChild {
  MoveOnly child;

  static HasMoveOnlyChild *create() {
    return new (malloc(sizeof(HasMoveOnlyChild))) HasMoveOnlyChild();
  }
};

HasMoveOnlyChild moveIntoResult(HasMoveOnlyChild &x) { return std::move(x); }

struct __attribute__((swift_attr("import_as_ref"))) PrivateCopyCtor {
  PrivateCopyCtor() = default;
  PrivateCopyCtor(PrivateCopyCtor &&) = default;

  int test() const { return 42; }
  int testMutable() { return 42; }

  static PrivateCopyCtor *create() {
    return new (malloc(sizeof(PrivateCopyCtor))) PrivateCopyCtor();
  }

private:
  PrivateCopyCtor(const PrivateCopyCtor &) {}
};

PrivateCopyCtor moveIntoResult(PrivateCopyCtor &x) { return std::move(x); }

struct __attribute__((swift_attr("import_as_ref"))) BadCopyCtor {
  BadCopyCtor() = default;
  BadCopyCtor(BadCopyCtor &&) = default;
  BadCopyCtor(const BadCopyCtor &) { __builtin_trap(); }

  int test() const { return 42; }
  int testMutable() { return 42; }

  static BadCopyCtor *create() {
    return new (malloc(sizeof(BadCopyCtor))) BadCopyCtor();
  }
};

BadCopyCtor moveIntoResult(BadCopyCtor &x) { return std::move(x); }

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_MOVE_ONLY_H
