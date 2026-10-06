// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/copyable-value-moves.swift -module-name Moves -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/moves.h
// RUN: %target-interop-build-clangxx -std=c++14 -fno-elide-constructors -c %s -I %t -o %t/moves.o
// RUN: %target-interop-build-swift %S/copyable-value-moves.swift -module-name Moves -Xfrontend -entry-point-function-name -Xfrontend swiftMain -Xlinker %t/moves.o -o %t/moves
// RUN: %target-codesign %t/moves
// RUN: %target-run %t/moves
// RUN: not --crash %target-run %t/moves borrow
// RUN: not --crash %target-run %t/moves generic
// RUN: not --crash %target-run %t/moves inout
// RUN: not --crash %target-run %t/moves member
// RUN: not --crash %target-run %t/moves enum
// RUN: not --crash %target-run %t/moves array
// RUN: not --crash %target-run %t/moves optional

// Repeat with opaque layouts for Value and Choice.
// RUN: %target-swift-frontend %S/copyable-value-moves.swift -module-name Moves -enable-library-evolution -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/moves.h
// RUN: %target-interop-build-clangxx -std=c++14 -fno-elide-constructors -c %s -I %t -o %t/moves.o
// RUN: %target-interop-build-swift %S/copyable-value-moves.swift -module-name Moves -enable-library-evolution -Xfrontend -entry-point-function-name -Xfrontend swiftMain -Xlinker %t/moves.o -o %t/moves
// RUN: %target-codesign %t/moves
// RUN: %target-run %t/moves
// RUN: not --crash %target-run %t/moves borrow
// RUN: not --crash %target-run %t/moves generic
// RUN: not --crash %target-run %t/moves inout
// RUN: not --crash %target-run %t/moves member
// RUN: not --crash %target-run %t/moves enum
// RUN: not --crash %target-run %t/moves array
// RUN: not --crash %target-run %t/moves optional

// REQUIRES: executable_test
// The simulator runner reports crashes as ordinary exits.
// UNSUPPORTED: DARWIN_SIMULATOR={{.*}}

#include <assert.h>
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

static_assert(std::is_standard_layout<Moves::Value>::value,
              "the Swift storage stays at offset zero");
static_assert(std::is_nothrow_move_constructible<Moves::Value>::value, "");
static_assert(std::is_nothrow_move_assignable<Moves::Value>::value, "");

extern "C" size_t swift_retainCount(void *_Nonnull);
static size_t retainCount(const Moves::Ref &ref) {
  return swift_retainCount(
      swift::_impl::_impl_RefCountedClass::getOpaquePointer(ref));
}

// This works for inline and opaque wrappers. Named sources force moves even
// when the compiler would elide moves from temporary return values.
template <class T>
static void checkMoves(T value, const Moves::Ref &ref) {
  auto count = retainCount(ref);
  auto allocs = allocations;
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

  T copied(value);
  assert(retainCount(ref) > count);
  auto copiedCount = retainCount(ref);
  T assigned(value);
  assigned = value;
  assert(retainCount(ref) > copiedCount);
  assigned = copiedEmpty;
  assert(retainCount(ref) == copiedCount);
  copied = std::move(empty);
  assert(retainCount(ref) == count);

  copied = value; // Reinitialize a moved-from destination by copying.
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
  // Reallocation must also tolerate a moved-from element.
  values.push_back(std::move(last));
  assert(retainCount(ref) == count);
  std::vector<T> copiedValues(values);
  assert(retainCount(ref) > count);
}

int main(int argc, char **argv) {
  using namespace Moves;
  auto ref = Ref::init();
  assert(retainCount(ref) == 1);
  {
    auto value = Value::init(ref, 42);
    assert(value.check());
    assert(retainCount(ref) == 2);
    if (argc > 1) {
      auto moved = std::move(value);
      if (!strcmp(argv[1], "borrow"))
        (void)borrow(value);
      if (!strcmp(argv[1], "generic"))
        (void)borrowGeneric(value);
      if (!strcmp(argv[1], "inout"))
        update(value);
      if (!strcmp(argv[1], "member"))
        (void)value.check();
      if (!strcmp(argv[1], "enum")) {
        auto e = makeChoice(moved);
        auto f = std::move(e);
        (void)e.isValue();
      }
      if (!strcmp(argv[1], "array")) {
        auto a = makeArray(moved);
        auto b = std::move(a);
        (void)borrowGeneric(a);
      }
      if (!strcmp(argv[1], "optional")) {
        auto a = makeOptional(moved);
        auto b = std::move(a);
        (void)a.get();
      }
      return 0; // Each selected access above must trap first.
    }
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
    // Array copies share their elements, so check allocation transfer
    // separately.
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
