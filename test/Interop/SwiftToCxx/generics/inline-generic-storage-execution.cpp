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
#include <string>
#if defined(_WIN32)
#include <malloc.h>
#endif

static size_t allocations = 0;
static size_t liveAllocations = 0;

void *trackedAlloc(size_t size, size_t alignment) {
  ++allocations;
  ++liveAllocations;
#if defined(_WIN32)
  return _aligned_malloc(size, alignment);
#else
  void *pointer = nullptr;
  if (alignment < sizeof(void *))
    alignment = sizeof(void *);
  int result = posix_memalign(&pointer, alignment, size);
  assert(result == 0);
  return pointer;
#endif
}

void trackedFree(void *pointer) {
  --liveAllocations;
#if defined(_WIN32)
  _aligned_free(pointer);
#else
  free(pointer);
#endif
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

  // Small payload-dependent and resilient layouts do not need boxes either.
  {
    auto dependent = Dependent<int32_t>::init(42);
    assert(dependent.getValue() == 42);
    assert(allocations == 0);
    auto optional = swift::Optional<int32_t>::none();
    assert(optional.isNone());
    assert(allocations == 0);
    auto resilient = Resilient<int32_t>::init(9);
    assert(resilient.getValue() == 9);
    assert(allocations == 0);
    auto array = swift::Array<Resilient<int32_t>>::init();
    assert(allocations == 0);
    array.append(resilient);
    assert(array.getCount() == 1);
    assert(allocations == 0);
    auto wrapper = Wrapper<int32_t>::init(resilient);
    assert(wrapper.getValue().getValue() == 9);
  }

  // Optional construction, copying, assignment and consuming calls must use
  // value witnesses without relocating initialized inline storage as bytes.
  {
    using OptionalInt = swift::Optional<int32_t>;
    auto value = OptionalInt::some(42);
    auto copy = value;
    value = OptionalInt::none();
    assert(value.isNone() && copy.get() == 42);
    value = copy;
    auto &alias = value;
    value = alias;
    assert(value.get() == 42);
    auto returned = identity(value);
    consume(returned);
    assert(returned.get() == 42);
    resetOptional(returned);
    assert(returned.isNone());

    auto nested = swift::Optional<OptionalInt>::some(copy);
    assert(identity(nested).get().get() == 42);
    auto pair = swift::Optional<WordPair>::some(WordPair::init(23));
    assert(identity(pair).get().getB() == 24);
    auto string = makeOptionalString();
    assert(std::string(identity(string).get()) == "inline optional");
    auto array = swift::Optional<swift::Array<int32_t>>::some(makeArray());
    assert(identity(array).get()[1] == 22);
    consume(array);
    assert(array.get().getCount() == 2);
    assert(allocations == 0);
  }

  {
    auto value = makeTrackedOptional();
    auto copy = identity(value);
    consume(copy);
    resetOptional(value);
    assert(getLiveCount() == 1 && copy.isSome());
    copy = makeTrackedOptional();
    assert(getLiveCount() == 1);
    auto &alias = copy;
    copy = alias;
    assert(getLiveCount() == 1);
    resetOptional(copy);
    assert(getLiveCount() == 0);
    assert(allocations == 0);
  }

  // Size and alignment are independent reasons to fall back to a heap box.
  {
    auto large = swift::Optional<LargePayload>::some(LargePayload::init(31));
    assert(allocations == 1 && liveAllocations == 1);
    auto copy = identity(large);
    assert(copy.get().getD() == 31);
    assert(allocations == 2 && liveAllocations == 2);
    consume(copy);
    assert(liveAllocations == 2);
    copy = swift::Optional<LargePayload>::none();
    assert(copy.isNone() && large.get().getA() == 31);
    assert(liveAllocations == 2);
  }
  assert(liveAllocations == 0);
  {
    auto aligned = swift::Optional<AlignedByte>::some(AlignedByte::init(7));
    assert(liveAllocations == 1);
    auto copy = identity(aligned);
    assert(copy.get().getValue() == 7 && liveAllocations == 2);
    consume(copy);
    assert(liveAllocations == 2);
  }
  assert(liveAllocations == 0);
}
