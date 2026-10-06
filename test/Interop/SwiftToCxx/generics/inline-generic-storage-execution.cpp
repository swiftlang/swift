// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/inline-generic-storage.swift -module-name InlineStorage \
// RUN:   -cxx-interoperability-mode=default -enable-library-evolution \
// RUN:   -typecheck -verify \
// RUN:   -emit-clang-header-path %t/storage.h
// RUN: %target-interop-build-clangxx -std=c++17 -fno-exceptions -c %s -I %t -o %t/main.o
// RUN: %target-interop-build-swift %S/inline-generic-storage.swift -module-name InlineStorage \
// RUN:   -enable-library-evolution -o %t/main -Xlinker %t/main.o \
// RUN:   -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test

#include <cassert>
#include <cstddef>
#include <cstdlib>

static size_t allocations = 0;
static size_t liveAllocations = 0;

void *trackedAlloc(size_t size, size_t alignment) {
  // The types in this test need no greater alignment than malloc provides.
  assert(alignment <= alignof(std::max_align_t));
  ++allocations;
  ++liveAllocations;
  return malloc(size);
}

void trackedFree(void *pointer) {
  --liveAllocations;
  free(pointer);
}

#define SWIFT_CXX_INTEROPERABILITY_OVERRIDE_OPAQUE_STORAGE_alloc trackedAlloc
#define SWIFT_CXX_INTEROPERABILITY_OVERRIDE_OPAQUE_STORAGE_free trackedFree
#include "storage.h"

int main() {
  using namespace InlineStorage;

  // Creation, copying and mutation must not box the Array handle.
  {
    auto empty = swift::Array<int32_t>::init();
    assert(empty.getCount() == 0);
    auto array = makeArray();
    auto copy = array;
    copy.append(44);
    assert(array.getCount() == 2 && array[0] == 11 && array[1] == 22);
    assert(copy.getCount() == 3 && copy[2] == 44);
    empty = array;
    appendArray(empty);
    assert(empty.getCount() == 3 && empty[2] == 33);
    auto returned = passArray(array);
    assert(returned.getCount() == 2 && returned[1] == 22);
    assert(consumeArray(array) == 33);
    assert(array.getCount() == 2);
    auto generic = identity(array);
    assert(generic[0] == 11);
    consume(generic);
    assert(generic.getCount() == 2);
    assert(allocations == 0);
  }

  // A generic argument can itself be represented by an inline wrapper.
  {
    auto byte = InlineByte<int32_t>::init(7);
    auto copy = identity(byte);
    assert(copy.getValue() == 7);
    consume(copy);
    auto array = swift::Array<InlineByte<int32_t>>::init();
    array.append(byte);
    assert(array[0].getValue() == 7);
    auto nested = InlineArray<int32_t>::init(19);
    assert(nested.getValues()[0] == 19);
    auto large = InlineLarge<int32_t>::init(29);
    auto largeCopy = identity(large);
    assert(largeCopy.getValue() == 29 && largeCopy.getD() == 4);
    assert(alignof(decltype(large)) == 16);
    consume(largeCopy);
    assert(allocations == 0);
  }

  // A fixed-layout generic enum must still project its payload correctly.
  {
    auto value = InlineEnum<int32_t>::value(23);
    assert(value.isValue() && value.getValue() == 23);
    auto copy = identity(value);
    assert(copy.isValue() && copy.getValue() == 23);
    auto none = InlineEnum<int32_t>::none();
    assert(none.isNone());
    assert(allocations == 0);
  }

  // Storage changes must preserve the Swift value's ownership operations.
  {
    assert(getLiveCount() == 0);
    auto array = makeTrackedArray();
    auto copy = array;
    auto returned = identity(copy);
    consume(returned);
    assert(getLiveCount() == 1);
    assert(allocations == 0);
  }
  assert(getLiveCount() == 0);

  // Payload-dependent and resilient layouts must continue to use boxes.
  {
    auto dependent = Dependent<int32_t>::init(42);
    assert(dependent.getValue() == 42);
    assert(allocations == 1);
    auto optional = swift::Optional<int32_t>::none();
    assert(optional.isNone());
    assert(allocations == 2);
    auto resilient = Resilient<int32_t>::init(9);
    assert(resilient.getValue() == 9);
    assert(allocations == 3);
    auto array = swift::Array<Resilient<int32_t>>::init();
    assert(allocations == 3);
    array.append(resilient);
    assert(array.getCount() == 1);
    // append copies the boxed element before consuming the copy.
    assert(allocations == 4 && liveAllocations == 3);
    auto wrapper = Wrapper<int32_t>::init(resilient);
    assert(wrapper.getValue().getValue() == 9);
  }
  assert(liveAllocations == 0);
}
