// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/consuming-parameter-in-cxx.swift -module-name Init -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/consuming.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-consume-execution.o
// RUN: %target-interop-build-swift %S/consuming-parameter-in-cxx.swift -o %t/swift-consume-execution -Xlinker %t/swift-consume-execution.o -module-name Init -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-consume-execution
// RUN: %target-run %t/swift-consume-execution | %FileCheck %s

// RUN: %target-swift-frontend %S/consuming-parameter-in-cxx.swift -module-name Init -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/consuming.h -enable-experimental-feature GenerateConsumingValueParametersInCXX
// RUN: %target-interop-build-clangxx -std=c++17 -DTEST_CONSUMING_MOVES -c %s -I %t -o %t/swift-move-execution.o
// RUN: %target-interop-build-swift %S/consuming-parameter-in-cxx.swift -o %t/swift-move-execution -Xlinker %t/swift-move-execution.o -module-name Init -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/swift-move-execution
// RUN: %target-run %t/swift-move-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateConsumingValueParametersInCXX

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <type_traits>
#include <utility>

size_t allocCount = 0;
size_t totalAllocs = 0;

void * _Nonnull trackedAlloc(size_t size, size_t align) {
    ++allocCount;
    ++totalAllocs;
    return malloc(size);
}
void trackedFree(void *_Nonnull p) {
    --allocCount;
    free(p);
}

#define SWIFT_CXX_INTEROPERABILITY_OVERRIDE_OPAQUE_STORAGE_alloc trackedAlloc
#define SWIFT_CXX_INTEROPERABILITY_OVERRIDE_OPAQUE_STORAGE_free  trackedFree

#include "consuming.h"

extern "C" size_t swift_retainCount(void * _Nonnull obj);

size_t getRetainCount(const Init::AKlass & swiftClass) {
  void *p = swift::_impl::_impl_RefCountedClass::getOpaquePointer(swiftClass);
  return swift_retainCount(p);
}

#ifdef TEST_CONSUMING_MOVES
// A single by-value binding keeps an unambiguous function address and supports
// independent ownership choices for each consumed argument.
static_assert(std::is_same_v<decltype(&Init::consumeHandoff),
                             bool (*)(Init::HandoffValue) noexcept>);
static_assert(
    std::is_same_v<decltype(&Init::borrowHandoff),
                   swift::Int (*)(const Init::HandoffValue &) noexcept>);
static_assert(
    std::is_same_v<decltype(&Init::initializeSwiftValue<Init::HandoffValue>),
                   void (*)(void *, Init::HandoffValue) noexcept>);

static void testConsumingMoves() {
  using namespace Init;
  assert(handoffTokenCount() == 0);
  {
    auto value = HandoffValue::init();
    auto allocations = totalAllocs;
    assert(!consumeHandoff(value));
    assert(totalAllocs ==
           allocations + (swift::_impl::isOpaqueLayout<HandoffValue> ? 1 : 0));
    assert(isHandoffUnique(value));
    assert(borrowHandoff(std::move(value)) == 1);
    assert(isHandoffUnique(value));
    allocations = totalAllocs;
    assert(consumeHandoff(std::move(value)));
    assert(totalAllocs == allocations);
    assert(handoffTokenCount() == 0);
  }
  {
    const auto value = HandoffValue::init();
    assert(!consumeHandoff(std::move(value)));
    assert(handoffTokenCount() == 1);
  }
  assert(consumeHandoff(HandoffValue::init()));
  assert(handoffTokenCount() == 0);
  {
    auto first = HandoffValue::init();
    auto second = HandoffValue::init();
    assert(consumeHandoffs(first, second) == 0);
    assert(consumeHandoffs(first, std::move(second)) == 2);
    assert(handoffTokenCount() == 1);
    assert(consumeHandoffs(std::move(first), HandoffValue::init()) == 3);
    assert(handoffTokenCount() == 0);
  }
  {
    auto value = HandoffValue::init();
    auto copied =
        swift::_impl::implClassFor<HandoffValue>::type::returnNewValue(
            [&](char *destination) {
              initializeSwiftValue(destination, value);
            });
    assert(!isHandoffUnique(value));
    assert(!isHandoffUnique(copied));
  }
  {
    auto value = HandoffValue::init();
    auto allocations = totalAllocs;
    auto moved = swift::_impl::implClassFor<HandoffValue>::type::returnNewValue(
        [&](char *destination) {
          initializeSwiftValue(destination, std::move(value));
        });
    assert(isHandoffUnique(moved));
    // Only the result's storage is allocated; the handoff adds no allocation.
    assert(totalAllocs ==
           allocations + (swift::_impl::isOpaqueLayout<HandoffValue> ? 1 : 0));
    assert(handoffTokenCount() == 1);
  }
  assert(handoffTokenCount() == 0);
  assert(allocCount == 0);
}
#endif

#ifdef TEST_THROWING_HANDOFF
static void testThrowingHandoff() {
  using namespace Init;
#ifdef __cpp_exceptions
  // Before the call begins, an exception while preparing a later argument must
  // destroy both the already prepared Swift payload and its wrapper storage.
  try {
    alignas(HandoffValue) char buffer[sizeof(HandoffValue)];
    auto &prepared = *new (buffer) HandoffValue(HandoffValue::init());
    swift::_impl::ConsumedValueStorageDestroyer<HandoffValue> guard(prepared,
                                                                    false);
    assert(handoffTokenCount() == 1);
    throw 42;
  } catch (int) {
    assert(handoffTokenCount() == 0);
    assert(allocCount == 0);
  }
#endif
  auto value = HandoffValue::init();
#ifdef __cpp_exceptions
  assert(consumeHandoffThrowing(HandoffValue::init(), false));
  try {
    consumeHandoffThrowing(value, true);
    assert(false);
  } catch (const swift::Error &) {
    assert(isHandoffUnique(value));
  }
  try {
    consumeHandoffThrowing(std::move(value), true);
    assert(false);
  } catch (const swift::Error &) {
    assert(handoffTokenCount() == 0);
  }
#else
  auto success = consumeHandoffThrowing(HandoffValue::init(), false);
  assert(success.has_value() && success.value());
  auto copiedError = consumeHandoffThrowing(value, true);
  assert(!copiedError.has_value());
  assert(isHandoffUnique(value));
  auto movedError = consumeHandoffThrowing(std::move(value), true);
  assert(!movedError.has_value());
  assert(handoffTokenCount() == 0);
#endif
  assert(allocCount == 0);
}
#endif

int main() {
  using namespace Init;

#ifdef TEST_CONSUMING_MOVES
  testConsumingMoves();
#endif
#ifdef TEST_THROWING_HANDOFF
  testThrowingHandoff();
#endif

  {
    auto k = AKlass::init();
    k.takeKlass();
    assert(getRetainCount(k) == 1);
  }
// CHECK: destroy AKlass
  {
    auto k = AKlass::init();
    auto x = createSmallStructNonTrivial(k);
    auto x2 = InitFromSmall::init(x);
    assert(getRetainCount(k) == 2);
  }
// CHECK-NEXT: destroy AKlass
  {
    auto k = AKlass::init();
    auto x = createSmallStructNonTrivial(k);
    auto c = TheGenericContainer<SmallStructNonTrivial>::init(x);
    assert(getRetainCount(k) == 3);
    c.takeGenericContainer();
    assert(getRetainCount(k) == 3);
  }
// CHECK-NEXT: destroy AKlass
  // verify that all of the opaque buffers are freed.
  assert(allocCount == 0);
  assert(totalAllocs != 0);
  return 0;
}
