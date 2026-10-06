//===--- OptionalBench.cpp ---------------------------------------*- C++
//-*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <algorithm>
#include <chrono>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <optional>
#include <vector>

#ifdef COUNT_ALLOCATIONS
static size_t allocations = 0;

static void *trackedAlloc(size_t size, size_t alignment) {
  ++allocations;
  void *pointer = nullptr;
  if (posix_memalign(&pointer, std::max(alignment, sizeof(void *)), size))
    std::abort();
  return pointer;
}

#define SWIFT_CXX_INTEROPERABILITY_OVERRIDE_OPAQUE_STORAGE_alloc trackedAlloc
#define SWIFT_CXX_INTEROPERABILITY_OVERRIDE_OPAQUE_STORAGE_free free
#endif
#include "OptionalBench.h"

// Force the wrapper to exist in memory, including for trivial std::optional.
// This does not add a function call to every operation.
template <class T>
static void escape(const T &value) {
  asm volatile("" : : "r"(&value) : "memory");
}

template <class F>
static void measure(const char *name, size_t iterations, F action,
                    size_t operationsPerIteration = 1) {
  for (size_t i = 0; i < std::min(size_t(1000), iterations); ++i)
    action(i);
#ifdef COUNT_ALLOCATIONS
  allocations = 0;
  for (size_t i = 0; i < iterations; ++i)
    action(i);
  std::printf("%s,%.6f\n", name,
              double(allocations) / (iterations * operationsPerIteration));
#else
  std::vector<double> samples;
  for (size_t sample = 0; sample < 7; ++sample) {
    auto start = std::chrono::steady_clock::now();
    for (size_t i = 0; i < iterations; ++i)
      action(i);
    auto elapsed = std::chrono::steady_clock::now() - start;
    samples.push_back(
        std::chrono::duration<double, std::nano>(elapsed).count() /
        (iterations * operationsPerIteration));
  }
  std::sort(samples.begin(), samples.end());
  std::printf("%s,%.6f\n", name, samples[samples.size() / 2]);
#endif
}

int main(int argc, char **argv) {
  const size_t iterations =
      argc > 1 ? std::strtoull(argv[1], nullptr, 10) : 200000;
  if (!iterations)
    return 1;
  using OptionalInt = swift::Optional<int64_t>;
  auto integer = OptionalBench::integer(42);
  auto none = OptionalInt::none();
  auto destination = integer;
  auto string = OptionalBench::string();
  auto array = OptionalBench::array();
  auto plainArray = array.get();
  auto large = OptionalBench::large();
  auto aligned = OptionalBench::aligned();
  std::optional<int64_t> stdInteger = 42;
  if (integer.get() != 42 || !none.isNone() ||
      string.get().getCount() != 1024 || array.get()[1] != 22 ||
      large.get().getD() != 42 || aligned.get().getValue() != 7)
    return 1;
  std::fprintf(stderr, "swift::Optional<int64_t>: size=%zu align=%zu\n",
               sizeof(integer), alignof(decltype(integer)));
  std::fprintf(stderr, "std::optional<int64_t>: size=%zu align=%zu\n",
               sizeof(stdInteger), alignof(decltype(stdInteger)));

  measure("int_some", iterations, [](size_t i) {
    auto value = OptionalInt::some(i);
    escape(value);
  });
  measure("int_none", iterations, [](size_t) {
    auto value = OptionalInt::none();
    escape(value);
  });
  measure("int_swift_return", iterations, [](size_t i) {
    auto value = OptionalBench::integer(i);
    escape(value);
  });
  measure("int_copy", iterations, [&](size_t) {
    auto value = integer;
    escape(value);
  });
  // Assignment reuses storage on both revisions, including changes of case.
  measure("int_assign", iterations, [&](size_t i) {
    destination = i % 2 ? integer : none;
    escape(destination);
  });
  measure("string_copy", iterations, [&](size_t) {
    auto value = string;
    escape(value);
  });
  measure("array_copy", iterations, [&](size_t) {
    auto value = array;
    escape(value);
  });
  measure("large_fallback_copy", iterations, [&](size_t) {
    auto value = large;
    escape(value);
  });
  measure("aligned_fallback_copy", iterations, [&](size_t) {
    auto value = aligned;
    escape(value);
  });
  measure("plain_array_copy", iterations, [&](size_t) {
    auto value = plainArray;
    escape(value);
  });
  measure("std_int_some", iterations, [](size_t i) {
    std::optional<int64_t> value = i;
    escape(value);
  });
  measure("std_int_none", iterations, [](size_t) {
    std::optional<int64_t> value;
    escape(value);
  });
  measure("std_int_copy", iterations, [&](size_t) {
    auto value = stdInteger;
    escape(value);
  });

  // Traverse a live collection to include the wrapper footprint and
  // indirection. Construction and vector allocation are outside the
  // measurement.
  constexpr size_t elementCount = 65536;
  std::vector<OptionalInt> values;
  values.reserve(elementCount);
  for (size_t i = 0; i < elementCount; ++i)
    values.push_back(i % 2 ? OptionalInt::some(i) : OptionalInt::none());
  int64_t sum = 0;
  measure(
      "int_vector_traverse", std::max(size_t(1), iterations / elementCount),
      [&](size_t) {
        sum = 0;
        for (const auto &value : values)
          if (value.isSome())
            sum += value.get();
        escape(sum);
      },
      elementCount);
  if (sum != int64_t(elementCount / 2) * (elementCount / 2))
    return 1;
}
