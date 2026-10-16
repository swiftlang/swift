// RUN: %empty-directory(%t)

// Without copy elision, C++14 moves the objects that the factories create
// before Swift has initialized their values.
// RUN: %target-swift-frontend %S/copyable-value-moves.swift -module-name Moves -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/moves.h
// RUN: %target-interop-build-clangxx -std=c++14 -fno-elide-constructors -c %s -I %t -o %t/moves.o
// RUN: %target-interop-build-swift %S/copyable-value-moves.swift -module-name Moves -Xfrontend -entry-point-function-name -Xfrontend swiftMain -Xlinker %t/moves.o -o %t/moves
// RUN: %target-codesign %t/moves
// RUN: %target-run %t/moves

// A moved-from object holds no Swift value, so using it traps.
// RUN: not --crash %target-run %t/moves borrow 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves generic 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves inout 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves member 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves enum 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves array 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves optional 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s

// Repeat with boxed values.
// RUN: %target-swift-frontend %S/copyable-value-moves.swift -module-name Moves -enable-library-evolution -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/moves.h
// RUN: %target-interop-build-clangxx -std=c++14 -fno-elide-constructors -c %s -I %t -o %t/moves.o
// RUN: %target-interop-build-swift %S/copyable-value-moves.swift -module-name Moves -enable-library-evolution -Xfrontend -entry-point-function-name -Xfrontend swiftMain -Xlinker %t/moves.o -o %t/moves
// RUN: %target-codesign %t/moves
// RUN: %target-run %t/moves
// RUN: not --crash %target-run %t/moves borrow 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves generic 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves inout 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves member 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves enum 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves array 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves optional 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s

// REQUIRES: executable_test

// The simulator runner reports a crash as an ordinary exit, which the runs
// above that expect a trap can't tell apart from a failure.
// UNSUPPORTED: DARWIN_SIMULATOR={{.*}}

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <type_traits>
#include <utility>
#include <vector>

static size_t allocations = 0;
static size_t liveAllocations = 0;
static void *_Nonnull allocate(size_t size, size_t alignment) {
  ++allocations;
  ++liveAllocations;
  return malloc(size);
}
static void deallocate(void *_Nonnull p) {
  --liveAllocations;
  free(p);
}
#define SWIFT_CXX_INTEROPERABILITY_OVERRIDE_OPAQUE_STORAGE_alloc allocate
#define SWIFT_CXX_INTEROPERABILITY_OVERRIDE_OPAQUE_STORAGE_free deallocate
#include "moves.h"

using namespace Moves;

static_assert(std::is_nothrow_move_constructible<Value>::value, "");
static_assert(std::is_nothrow_move_assignable<Value>::value, "");
static_assert(!swift::_impl::isOpaqueLayout<Value> ||
                  sizeof(Value) == sizeof(void *),
              "a boxed value needs no state besides the box");
static_assert(std::is_standard_layout<Value>::value,
              "an inline Swift value stays at offset zero");

extern "C" size_t swift_retainCount(void *_Nonnull);
static size_t retainCount(const Ref &ref) {
  return swift_retainCount(
      swift::_impl::_impl_RefCountedClass::getOpaquePointer(ref));
}

// Each named source forces a move, as copy elision could otherwise remove it.
template <class T> static void checkMoves(T value, const Ref &ref) {
  auto count = retainCount(ref);
  auto allocs = allocations;
  auto live = liveAllocations;
  T moved(std::move(value));
  assert(retainCount(ref) == count);
  assert(allocations == allocs);

  T empty(std::move(value));
  T copiedEmpty(value);
  auto &movedAlias = moved;
  auto &emptyAlias = empty;
  moved = std::move(movedAlias);
  empty = std::move(emptyAlias);
  value = std::move(moved);
  assert(retainCount(ref) == count);
  assert(allocations == allocs);
  assert(liveAllocations == live);

  T copied(value);
  assert(retainCount(ref) > count);
  auto copiedCount = retainCount(ref);
  T assigned(value);
  assigned = value;
  assert(retainCount(ref) > copiedCount);
  // Assigning a moved-from object destroys the value and frees its box.
  assigned = copiedEmpty;
  assert(retainCount(ref) == copiedCount);
  copied = std::move(empty);
  assert(retainCount(ref) == count);
  assert(liveAllocations == live);

  copied = value; // Copy into a moved-from destination.
  assert(retainCount(ref) == copiedCount);
  copied = copiedEmpty;
  assert(retainCount(ref) == count);
  copied = std::move(value);
  std::swap(copied, value);
  assert(retainCount(ref) == count);

  std::vector<T> values;
  values.reserve(1);
  values.push_back(std::move(value));
  T last(std::move(values[0]));
  // Reallocation also moves the moved-from element.
  values.push_back(std::move(last));
  assert(retainCount(ref) == count);
  std::vector<T> copiedValues(values);
  assert(retainCount(ref) > count);
}

static void useMovedFrom(const char *mode, const Ref &ref) {
  auto value = Value::init(ref, 1);
  auto moved = std::move(value);
  auto choice = makeChoice(moved);
  auto movedChoice = std::move(choice);
  auto array = makeArray(moved);
  auto movedArray = std::move(array);
  auto optional = makeOptional(moved);
  auto movedOptional = std::move(optional);
  // MOVED-FROM: about to use a moved-from value
  printf("about to use a moved-from value\n");
  fflush(stdout);
  if (!strcmp(mode, "borrow"))
    (void)borrow(value);
  else if (!strcmp(mode, "generic"))
    (void)borrowGeneric(value);
  else if (!strcmp(mode, "inout"))
    update(value);
  else if (!strcmp(mode, "member"))
    (void)value.check();
  else if (!strcmp(mode, "enum"))
    (void)choice.isValue();
  else if (!strcmp(mode, "array"))
    (void)borrowGeneric(array);
  else if (!strcmp(mode, "optional"))
    (void)optional.get();
  // MOVED-FROM-NOT: unreachable
  printf("unreachable\n");
}

int main(int argc, char **argv) {
  auto ref = Ref::init();
  assert(retainCount(ref) == 1);
  if (argc > 1) {
    useMovedFrom(argv[1], ref);
    return 0;
  }
  {
    auto value = Value::init(ref, 42);
    assert(value.check());
    assert(retainCount(ref) == 2);
    checkMoves(std::move(value), ref);
    assert(retainCount(ref) == 1);

    {
      auto source = Value::init(ref, 1);
      auto moved = std::move(source);
      assert(moved.check());
      const auto &constant = moved;
      auto copied = std::move(constant); // A const rvalue still copies.
      assert(retainCount(ref) == 3);
      assert(copied.check());
    }
    assert(retainCount(ref) == 1);

    value = Value::init(ref, 43);
    assert(value.check());
    auto result = identity(value);
    assert(result.check());
    assert(borrow(result) == 43);
    update(result);
    assert(borrow(result) == 44);
    assert(borrow(value) == 43);
    checkMoves(makeChoice(value), ref);
    auto number = Choice::number(42);
    auto movedNumber = std::move(number);
    assert(movedNumber.isNumber());
    checkMoves(makeOptional(value), ref);

    // Array copies share their elements, so check its moves separately.
    auto array = makeArray(value);
    auto count = retainCount(ref);
    auto allocs = allocations;
    auto moved = std::move(array);
    assert(allocations == allocs);
    assert(retainCount(ref) == count);
    assert(moved.getCount() == 1);
    array = std::move(moved);
    auto element = array[0];
    assert(element.check());
    assert(borrow(element) == 43);
  }
  assert(retainCount(ref) == 1);
  assert(liveAllocations == 0);
  return 0;
}
