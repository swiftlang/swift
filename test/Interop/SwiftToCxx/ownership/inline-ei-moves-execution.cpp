// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/inline-ei-moves.swift -module-name InlineMoves -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/moves.h
// RUN: %target-interop-build-clangxx -std=c++14 -fno-elide-constructors -c %s -I %t -o %t/moves.o
// RUN: %target-interop-build-swift %S/inline-ei-moves.swift -module-name InlineMoves -Xfrontend -entry-point-function-name -Xfrontend swiftMain -Xlinker %t/moves.o -o %t/moves
// RUN: %target-codesign %t/moves
// RUN: %target-run %t/moves
// RUN: not --crash %target-run %t/moves member 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves generic 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves generic-inout 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves consume 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves enum 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves boolean 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: %target-interop-build-clangxx -std=c++17 -c %s -I %t -o %t/moves17.o
// RUN: %target-interop-build-swift %S/inline-ei-moves.swift -module-name InlineMoves -Xfrontend -entry-point-function-name -Xfrontend swiftMain -Xlinker %t/moves17.o -o %t/moves17
// RUN: %target-codesign %t/moves17
// RUN: %target-run %t/moves17

// Frozen layouts remain inline with library evolution enabled.
// RUN: %target-swift-frontend %S/inline-ei-moves.swift -module-name InlineMoves -enable-library-evolution -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/moves.h
// RUN: %target-interop-build-clangxx -std=c++14 -fno-elide-constructors -c %s -I %t -o %t/moves.o
// RUN: %target-interop-build-swift %S/inline-ei-moves.swift -module-name InlineMoves -enable-library-evolution -Xfrontend -entry-point-function-name -Xfrontend swiftMain -Xlinker %t/moves.o -o %t/moves
// RUN: %target-codesign %t/moves
// RUN: %target-run %t/moves
// RUN: not --crash %target-run %t/moves member 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves generic 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves generic-inout 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves consume 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves enum 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: not --crash %target-run %t/moves boolean 2>&1 | %FileCheck --check-prefix=MOVED-FROM %s
// RUN: %target-interop-build-clangxx -std=c++17 -c %s -I %t -o %t/moves17.o
// RUN: %target-interop-build-swift %S/inline-ei-moves.swift -module-name InlineMoves -enable-library-evolution -Xfrontend -entry-point-function-name -Xfrontend swiftMain -Xlinker %t/moves17.o -o %t/moves17
// RUN: %target-codesign %t/moves17
// RUN: %target-run %t/moves17

// REQUIRES: executable_test
// UNSUPPORTED: DARWIN_SIMULATOR={{.*}}

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>
#include "moves.h"

using namespace InlineMoves;

static_assert(sizeof(SingleRef) == sizeof(void *), "no added state storage");
static_assert(sizeof(ReferenceChoice) == sizeof(void *), "no added state storage");
static_assert(std::is_nothrow_move_constructible<SingleRef>::value, "");
static_assert(std::is_nothrow_move_assignable<SingleRef>::value, "");
static_assert(std::is_trivially_copyable<TrivialBool>::value, "");
static_assert(std::is_trivially_destructible<TrivialBool>::value, "");
static_assert(!swift::_impl::isOpaqueLayout<NoExtraInhabitants>, "");
static_assert(!swift::_impl::isOpaqueLayout<PaddedRef>, "");
static_assert(std::is_standard_layout<PaddedRef>::value, "");

extern "C" size_t swift_retainCount(void *_Nonnull);
static size_t retainCount(const Ref &ref) {
  return swift_retainCount(
      swift::_impl::_impl_RefCountedClass::getOpaquePointer(ref));
}

// A named source requires a move even when copy elision is available.
template <class T> static void checkMoves(T value, const Ref &ref) {
  auto count = retainCount(ref);
  T moved(std::move(value));
  assert(moved.check());
  assert(retainCount(ref) == count);

  T empty(std::move(value));
  T copiedEmpty(value);
  auto &movedAlias = moved;
  auto &emptyAlias = empty;
  moved = std::move(movedAlias);
  empty = std::move(emptyAlias);
  value = std::move(moved);
  assert(value.check());
  assert(retainCount(ref) == count);

  T copied(value);
  assert(copied.check());
  assert(retainCount(ref) == count + 1);
  T assigned(value);
  assigned = value;
  auto &copyAlias = assigned;
  assigned = copyAlias;
  assert(assigned.check());
  assert(retainCount(ref) == count + 2);
  assigned = copiedEmpty;
  assert(retainCount(ref) == count + 1);
  copied = std::move(empty);
  assert(retainCount(ref) == count);

  copied = value;
  assert(copied.check());
  assert(retainCount(ref) == count + 1);
  copied = copiedEmpty;
  assert(retainCount(ref) == count);
  copied = std::move(value);
  std::swap(copied, value);
  assert(value.check());
  assert(retainCount(ref) == count);

  std::vector<T> values;
  values.reserve(1);
  values.push_back(std::move(value));
  T last(std::move(values[0]));
  values.push_back(std::move(last));
  assert(values.back().check());
  assert(retainCount(ref) == count);
  std::vector<T> copiedValues(values);
  assert(copiedValues.back().check());
  assert(retainCount(ref) == count + 1);
}

static void useMovedFrom(const char *mode, const Ref &ref) {
  auto value = SingleRef::init(ref);
  auto live = std::move(value);
  auto choice = ReferenceChoice::value(ref);
  auto liveChoice = std::move(choice);
  auto boolean = BoolWeak::init(true, ref);
  auto liveBoolean = std::move(boolean);
  // MOVED-FROM: about to use a moved-from value
  printf("about to use a moved-from value\n");
  fflush(stdout);
  if (!strcmp(mode, "member"))
    (void)value.check();
  else if (!strcmp(mode, "generic"))
    (void)borrowGeneric(value);
  else if (!strcmp(mode, "generic-inout"))
    updateGeneric(value);
  else if (!strcmp(mode, "consume"))
    (void)consume(value);
  else if (!strcmp(mode, "enum"))
    (void)choice.isValue();
  else if (!strcmp(mode, "boolean"))
    (void)boolean.isReady();
  // MOVED-FROM-NOT: unreachable
  printf("unreachable\n");
}

int main(int argc, char **argv) {
  {
    auto ref = Ref::init(42);
    assert(retainCount(ref) == 1);
    if (argc > 1) {
      useMovedFrom(argv[1], ref);
      return 0;
    }

    assert(sizeof(PaddedRef) == paddedStride());
    assert(sizeof(BoolWeak) == boolWeakStride());
    checkMoves(SingleRef::init(ref), ref);
    assert(retainCount(ref) == 1);
    checkMoves(PaddedRef::init(ref), ref);
    assert(retainCount(ref) == 1);
    checkMoves(ReferenceChoice::value(ref), ref);
    assert(retainCount(ref) == 1);

    auto choice = ReferenceChoice::empty();
    auto movedChoice = std::move(choice);
    assert(movedChoice.isEmpty());

    {
      auto choice = ReferenceChoice::value(ref);
      auto payload = choice.getValue();
      assert(payload.getId() == 42);
      assert(retainCount(ref) == 3);
    }
    assert(retainCount(ref) == 1);

    {
      auto value = PaddedRef::init(ref);
      auto result = identity(value);
      assert(result.check());
      assert(borrow(result) == 7);
      update(result);
      assert(result.getPrefix() == 8);
      assert(value.getPrefix() == 7);
    }
    assert(retainCount(ref) == 1);

    {
      auto value = SingleRef::init(ref);
      const auto &constant = value;
      auto copied = std::move(constant);
      assert(copied.check());
      assert(value.check());
      assert(retainCount(ref) == 3);
    }
    assert(retainCount(ref) == 1);

    auto value = BoolWeak::init(true, ref);
    auto moved = std::move(value);
    auto empty = std::move(value);
    auto copiedEmpty = empty;
    assert(moved.isReady());
    assert(moved.hasRef());
    copiedEmpty = moved;
    assert(copiedEmpty.hasRef());
    moved = empty;
    moved = BoolWeak::init(false, ref);
    assert(!moved.isReady());
    assert(moved.hasRef());

#if __cplusplus >= 201703L
    // The existing copy-only factory path needs C++17 copy elision while its
    // uninitialized result is being created.
    auto fallback = NoExtraInhabitants::init(ref);
    auto copiedFallback = std::move(fallback);
    assert(fallback.check());
    assert(copiedFallback.check());
#endif
    assert(retainCount(ref) == 1);
  }
  assert(destroyedRefs() == 1);

  swift::String cString("string from C++");
  swift::String emptyString(nullptr);
  std::string standard(80, 'a');
  swift::String standardString(standard);
  assert(std::string(cString) == "string from C++");
  assert(std::string(emptyString).empty());
  assert(std::string(standardString) == standard);

  auto trivial = TrivialBool::init(true);
  auto copiedTrivial = std::move(trivial);
  assert(trivial.getValue());
  assert(copiedTrivial.getValue());
  return 0;
}
