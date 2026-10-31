// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/noncopyable-type-in-cxx.swift -module-name Noncopyable -enable-experimental-feature GenerateBindingsForNoncopyableTypesInCXX -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/noncopyable.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-noncopyable-execution.o
// RUN: %target-interop-build-swift %S/noncopyable-type-in-cxx.swift -o %t/swift-noncopyable-execution -Xlinker %t/swift-noncopyable-execution.o -module-name Noncopyable -Xfrontend -enable-experimental-feature -Xfrontend GenerateBindingsForNoncopyableTypesInCXX -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-noncopyable-execution
// RUN: %target-run %t/swift-noncopyable-execution | %FileCheck %s

// A moved-from object holds no Swift value, so handing it to Swift traps.
// RUN: not --crash %target-run %t/swift-noncopyable-execution borrow-moved-from 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/swift-noncopyable-execution consume-moved-from 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/swift-noncopyable-execution call-on-moved-from 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/swift-noncopyable-execution call-on-moved-from-generic 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/swift-noncopyable-execution borrow-moved-from-generic 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s

// Repeat with boxed values.
// RUN: %target-swift-frontend %S/noncopyable-type-in-cxx.swift -module-name Noncopyable -enable-library-evolution -enable-experimental-feature GenerateBindingsForNoncopyableTypesInCXX -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/noncopyable.h
// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-noncopyable-execution.o
// RUN: %target-interop-build-swift %S/noncopyable-type-in-cxx.swift -enable-library-evolution -o %t/swift-noncopyable-execution -Xlinker %t/swift-noncopyable-execution.o -module-name Noncopyable -Xfrontend -enable-experimental-feature -Xfrontend GenerateBindingsForNoncopyableTypesInCXX -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/swift-noncopyable-execution
// RUN: %target-run %t/swift-noncopyable-execution | %FileCheck %s
// RUN: not --crash %target-run %t/swift-noncopyable-execution borrow-moved-from 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/swift-noncopyable-execution consume-moved-from 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/swift-noncopyable-execution call-on-moved-from 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/swift-noncopyable-execution call-on-moved-from-generic 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/swift-noncopyable-execution borrow-moved-from-generic 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateBindingsForNoncopyableTypesInCXX

// The simulator runner reports a crash as an ordinary exit, which the runs
// above that expect a trap can't tell apart from a failure.
// UNSUPPORTED: DARWIN_SIMULATOR={{.*}}

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <type_traits>
#include <utility>
#include <vector>

#include "noncopyable.h"

using namespace Noncopyable;

static_assert(!std::is_copy_constructible<MoveOnlyStruct>::value,
              "a noncopyable Swift type must not be copy constructible");
static_assert(!std::is_copy_assignable<MoveOnlyStruct>::value,
              "a noncopyable Swift type must not be copy assignable");
static_assert(std::is_move_constructible<MoveOnlyStruct>::value,
              "a noncopyable Swift type must be move constructible");
static_assert(std::is_move_assignable<MoveOnlyStruct>::value,
              "a noncopyable Swift type must be move assignable");

static_assert(!std::is_copy_constructible<Unique<swift::Int>>::value,
              "a generic noncopyable Swift type must not be copy constructible");
static_assert(std::is_nothrow_move_constructible<Unique<swift::Int>>::value,
              "a generic noncopyable Swift type must be move constructible");
static_assert(!std::is_copy_constructible<Handle<swift::Int>>::value,
              "a generic noncopyable Swift type must not be copy constructible");
static_assert(!swift::isUsableInGenericContext<Unique<swift::Int>>,
              "a noncopyable type cannot be a generic argument");
static_assert(sizeof(Unique<swift::Int>) == sizeof(void *),
              "a boxed value needs no moved-from flag");

static_assert(std::is_copy_constructible<CopyableStruct>::value,
              "a copyable Swift type must still be copy constructible");

// The moved-from flag is stored after the Swift value, so the Swift value is
// still at offset zero and can be handed to Swift as-is. A boxed value needs no
// flag.
static_assert(swift::_impl::isOpaqueLayout<MoveOnlyStruct>
                  ? sizeof(MoveOnlyStruct) == sizeof(void *)
                  : sizeof(MoveOnlyStruct) > 2 * sizeof(swift::Int),
              "the moved-from flag is stored next to the Swift value");
static_assert(std::is_standard_layout<MoveOnlyStruct>::value,
              "the Swift value must stay at offset zero");

static void useMovedFrom(const char *mode) {
  auto s = makeMoveOnlyStruct(1);
  auto t = std::move(s);
  auto e = MoveOnlyEnum::a(1);
  auto f = std::move(e);
  auto u = Unique<swift::Int>::init(1);
  auto v = std::move(u);
  auto h = Handle<swift::Int>::init(1);
  auto i = std::move(h);
  // MOVED-FROM: about to use a moved-from value
  printf("about to use a moved-from value\n");
  fflush(stdout);
  if (!strcmp(mode, "borrow-moved-from"))
    (void)borrowMoveOnlyStruct(s);
  else if (!strcmp(mode, "consume-moved-from"))
    consumeMoveOnlyStruct(std::move(s));
  else if (!strcmp(mode, "call-on-moved-from"))
    (void)e.isA();
  else if (!strcmp(mode, "call-on-moved-from-generic"))
    (void)h.getRaw();
  else if (!strcmp(mode, "borrow-moved-from-generic"))
    (void)borrowUnique(u);
  // MOVED-FROM-NOT: unreachable
  printf("unreachable\n");
}

int main(int argc, char **argv) {
  if (argc > 1) {
    useMovedFrom(argv[1]);
    return 0;
  }

  // A value returned from Swift is live, so its deinit runs once.
  // CHECK: destroy MoveOnlyStruct(42)
  {
    auto s = makeMoveOnlyStruct(42);
    assert(borrowMoveOnlyStruct(s) == 42);
  }

  // Move construction transfers ownership; the moved-from object is inert, so
  // the value is destroyed exactly once.
  // CHECK-NEXT: destroy MoveOnlyStruct(7)
  {
    auto s = makeMoveOnlyStruct(7);
    auto moved = std::move(s);
    assert(borrowMoveOnlyStruct(moved) == 7);
  }

  {
    auto a = makeMoveOnlyStruct(1);
    auto b = makeMoveOnlyStruct(2);

    // Assigning into a live object destroys the value it held.
    // CHECK-NEXT: destroy MoveOnlyStruct(1)
    a = std::move(b);
    assert(borrowMoveOnlyStruct(a) == 2);

    // Assigning into a moved-from object just initializes it.
    b = std::move(a);
    assert(borrowMoveOnlyStruct(b) == 2);

    // 'b' holds the only live value; 'a' is moved from.
    // CHECK-NEXT: destroy MoveOnlyStruct(2)
  }

  // Consuming hands ownership to Swift, so the C++ object must not destroy the
  // value a second time.
  // CHECK-NEXT: destroy MoveOnlyStruct(3)
  {
    auto s = makeMoveOnlyStruct(3);
    consumeMoveOnlyStruct(std::move(s));
  }

  // 'std::swap' assigns into a moved-from destination twice, so the destination
  // must not be destroyed when it holds no value.
  // CHECK-NEXT: destroy MoveOnlyStruct(20)
  // CHECK-NEXT: destroy MoveOnlyStruct(21)
  {
    auto a = makeMoveOnlyStruct(20);
    auto b = makeMoveOnlyStruct(21);
    std::swap(a, b);
    assert(borrowMoveOnlyStruct(a) == 21);
    assert(borrowMoveOnlyStruct(b) == 20);
  }

  // The standard library moves from moved-from objects as well: here a vector
  // reallocates while one of its elements is moved from.
  // CHECK-NEXT: destroy MoveOnlyStruct(40)
  // CHECK-NEXT: destroy MoveOnlyStruct(41)
  {
    std::vector<MoveOnlyStruct> v;
    v.reserve(1);
    v.push_back(makeMoveOnlyStruct(40));
    auto x = std::move(v[0]);
    v.push_back(makeMoveOnlyStruct(41));
  }

  // Self assignment keeps the value.
  // CHECK-NEXT: destroy MoveOnlyStruct(30)
  {
    auto a = makeMoveOnlyStruct(30);
    auto &alias = a;
    a = std::move(alias);
    assert(borrowMoveOnlyStruct(a) == 30);
  }

  // A class reference the value owns is released exactly once, even though the
  // value is moved and then consumed by Swift.
  // CHECK-NEXT: destroy Ref
  {
    auto s = makeMoveOnlyWithRef();
    auto t = std::move(s);
    consumeMoveOnlyWithRef(std::move(t));
  }

  // A value of more than four words is returned and consumed indirectly.
  // CHECK-NEXT: destroy BigMoveOnly(4)
  {
    auto b = makeBigMoveOnly(4);
    auto c = std::move(b);
    assert(c.getA() == 4);
    consumeBigMoveOnly(std::move(c));
  }

  // A temporary binds directly to a consuming parameter.
  // CHECK-NEXT: destroy MoveOnlyStruct(50)
  consumeMoveOnlyStruct(makeMoveOnlyStruct(50));

  // Growing a vector moves the elements and destroys the moved-from ones. The
  // elements are popped because the order a vector destroys them in is
  // unspecified.
  // CHECK-NEXT: destroy MoveOnlyStruct(61)
  // CHECK-NEXT: destroy MoveOnlyStruct(60)
  {
    std::vector<MoveOnlyStruct> v;
    v.push_back(makeMoveOnlyStruct(60));
    v.push_back(makeMoveOnlyStruct(61));
    assert(borrowMoveOnlyStruct(v[0]) == 60);
    v.pop_back();
    v.pop_back();
  }

  // A noncopyable enum can be constructed, inspected, switched over and moved.
  {
    auto e = MoveOnlyEnum::a(42);
    assert(e.isA());
    assert(!e.isB());

    auto f = std::move(e);
    assert(f.isA());
    switch (f) {
    case MoveOnlyEnum::cases::a:
      break;
    case MoveOnlyEnum::cases::b:
    case MoveOnlyEnum::cases::c:
      assert(false);
      break;
    }

    auto g = MoveOnlyEnum::b();
    assert(g.isB());
  }

  // A payload the enum owns is released exactly once after a move.
  // CHECK-NEXT: destroy Ref
  {
    auto e = MoveOnlyEnum::c(Ref::init());
    auto f = std::move(e);
    assert(f.isC());
  }

  // A generic noncopyable value is moved and consumed like the others.
  // CHECK-NEXT: destroy Unique
  {
    auto u = Unique<swift::Int>::init(5);
    assert(u.get() == 5);
    u.set(6);
    auto v = std::move(u);
    assert(borrowUnique(v) == 6);
    consumeUnique(std::move(v));
  }

  // Assigning into a live object destroys its value, and the class reference
  // that the value owns.
  // CHECK-NEXT: destroy Unique
  // CHECK-NEXT: destroy Ref
  // CHECK-NEXT: assigned
  // CHECK-NEXT: destroy Unique
  // CHECK-NEXT: destroy Ref
  {
    auto a = makeUnique(Ref::init());
    auto b = makeUnique(Ref::init());
    std::swap(a, b);
    a = std::move(b);
    printf("assigned\n");
  }

  // A vector reallocates while one of its elements is moved from.
  // CHECK-NEXT: destroy Handle(70)
  // CHECK-NEXT: destroy Handle(71)
  {
    std::vector<Handle<swift::Int>> v;
    v.reserve(1);
    v.push_back(Handle<swift::Int>::init(70));
    auto h = std::move(v[0]);
    v.push_back(Handle<swift::Int>::init(71));
    assert(h.getRaw() == 70);
    consumeHandle(std::move(h));
    v.pop_back();
    v.pop_back();
  }

  // A concrete specialization is passed and returned directly.
  // CHECK-NEXT: destroy Unique
  {
    auto u = makeUniqueInt(7);
    auto v = std::move(u);
    assert(borrowUniqueInt(v) == 7);
    consumeUniqueInt(std::move(v));
  }

  {
    auto f = Flag<swift::Int>::on(1);
    auto g = std::move(f);
    assert(g.isOn());
    switch (g) {
    case Flag<swift::Int>::cases::on:
      break;
    case Flag<swift::Int>::cases::off:
      assert(false);
      break;
    }
  }

  {
    auto s = Slot<swift::Int>::full(1);
    auto t = std::move(s);
    assert(t.isFull());
    auto e = Slot<swift::Int>::empty();
    switch (e) {
    case Slot<swift::Int>::cases::full:
      assert(false);
      break;
    case Slot<swift::Int>::cases::empty:
      break;
    }
  }

  // CHECK-NEXT: done
  printf("done\n");
  return 0;
}
