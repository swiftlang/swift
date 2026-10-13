//===--- Atomic.h - Utilities for atomic operations. ------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Utilities for atomic operations, to use with std::atomic.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_ATOMIC_H
#define SWIFT_RUNTIME_ATOMIC_H

#include "swift/Runtime/Config.h"
#include <assert.h>
#include <atomic>
#include <cstdlib>
#if defined(_WIN64)
#include <intrin.h>
#endif
#if (defined(__x86_64__) || defined(_M_X64)) && defined(__clang__) &&        \
    !defined(__AVX__)
#include <cpuid.h>
#endif


// FIXME: Workaround for rdar://problem/18889711. 'Consume' does not require
// a barrier on ARM64, but LLVM doesn't know that. Although 'relaxed'
// is formally UB by C++11 language rules, we should be OK because neither
// the processor model nor the optimizer can realistically reorder our uses
// of 'consume'.
#if defined(__arm__) || defined(_M_ARM) || defined(__arm64__) || defined(__aarch64__) || defined(_M_ARM64)
#  define SWIFT_MEMORY_ORDER_CONSUME (std::memory_order_relaxed)
#else
#  define SWIFT_MEMORY_ORDER_CONSUME (std::memory_order_consume)
#endif


#if defined(_M_ARM) || defined(_M_ARM64) || defined(__arm__) || defined(__aarch64__)
#define SWIFT_HAS_MSVC_ARM_ATOMICS 1
#else
#define SWIFT_HAS_MSVC_ARM_ATOMICS 0
#endif

namespace swift {
namespace impl {

#if (defined(__x86_64__) || defined(_M_X64)) && defined(__clang__)
#define SWIFT_ATOMIC_VECTOR_LOAD16 1
#if !defined(__AVX__)
/// Whether this processor has atomic 16-byte vector loads (see
/// has16ByteAtomicVectorLoads): unknown until the first 16-byte load asks
/// CPUID. A single byte, so the common path is one load with no
/// initialization guard; threads that race to set it agree on the value.
enum : unsigned char {
  VectorLoad16Unknown,
  VectorLoad16Unsupported,
  VectorLoad16Supported,
};
inline std::atomic<unsigned char> vectorLoad16Support{VectorLoad16Unknown};

__attribute__((noinline, cold))
inline bool detect16ByteAtomicVectorLoads() {
  unsigned a, b, c, d;
  bool supported = __get_cpuid(1, &a, &b, &c, &d) && (c & bit_AVX) != 0;
  vectorLoad16Support.store(supported ? VectorLoad16Supported
                                      : VectorLoad16Unsupported,
                            std::memory_order_relaxed);
  return supported;
}
#endif

/// Intel and AMD guarantee that aligned 16-byte SSE loads (MOVDQA and
/// friends) are atomic on every processor that enumerates AVX (Intel SDM
/// vol. 3A 9.1.1, AMD APM vol. 2 7.3.2); libatomic relies on the same rule.
/// Without it the only atomic 16-byte load is LOCK CMPXCHG16B, which takes
/// the cache line exclusive like a store and costs as much as one: it was
/// most of the time spent reading a task's status.
inline bool has16ByteAtomicVectorLoads() {
#if defined(__AVX__)
  // Built for AVX: every processor this code runs on has them.
  return true;
#else
  auto support = vectorLoad16Support.load(std::memory_order_relaxed);
  if (__builtin_expect(support == VectorLoad16Supported, true))
    return true;
  if (support == VectorLoad16Unsupported)
    return false;
  return detect16ByteAtomicVectorLoads();
#endif
}

template <class Value>
inline Value vectorLoad16(const volatile void *address) {
  static_assert(sizeof(Value) == 16, "16-byte values only");
  typedef long long vector16 __attribute__((vector_size(16)));
  vector16 bits;
  // The memory clobber keeps the compiler from moving accesses across the
  // load; x86 itself gives every load acquire semantics.
  __asm__ __volatile__("movdqa %1, %0"
                       : "=x"(bits)
                       : "m"(*(const volatile vector16 *)address)
                       : "memory");
  Value result;
  __builtin_memcpy(&result, &bits, sizeof(result));
  return result;
}
#else
#define SWIFT_ATOMIC_VECTOR_LOAD16 0
#endif

/// The default implementation for swift::atomic<T>, which just wraps
/// std::atomic with minor differences.
///
/// TODO: should we make this use non-atomic operations when the runtime
/// is single-threaded?
template <class Value, std::size_t Size = sizeof(Value)>
class alignas(Size) atomic_impl {
  std::atomic<Value> value;
public:
  constexpr atomic_impl(Value value) : value(value) {}

  /// Force clients to always pass an order.
  Value load(std::memory_order order) const {
#if SWIFT_ATOMIC_VECTOR_LOAD16
    if constexpr (Size == 16) {
      if (__builtin_expect(has16ByteAtomicVectorLoads(), true))
        return vectorLoad16<Value>(&value);
    }
#endif
    return value.load(order);
  }

  /// Force clients to always pass an order.
  void store(Value newValue, std::memory_order order) {
    return value.store(newValue, order);
  }

  /// Force clients to always pass an order.
  bool compare_exchange_weak(Value &oldValue, Value newValue,
                             std::memory_order successOrder,
                             std::memory_order failureOrder) {
    return value.compare_exchange_weak(oldValue, newValue, successOrder,
                                       failureOrder);
  }

  bool compare_exchange_strong(Value &oldValue, Value newValue,
                               std::memory_order successOrder,
                               std::memory_order failureOrder) {
    return value.compare_exchange_strong(oldValue, newValue, successOrder,
                                         failureOrder);
  }
};

#if defined(_WIN64)

/// MSVC's std::atomic uses an inline spin lock for 16-byte atomics,
/// which is not only unnecessarily inefficient but also doubles the size
/// of the atomic object.  We don't care about supporting ancient
/// AMD processors that lack cmpxchg16b, so we just use the intrinsic.
template <class Value>
class alignas(2 * sizeof(void*)) atomic_impl<Value, 2 * sizeof(void*)> {
  mutable volatile Value atomicValue;
public:
  constexpr atomic_impl(Value initialValue) : atomicValue(initialValue) {}

  atomic_impl(const atomic_impl &) = delete;
  atomic_impl &operator=(const atomic_impl &) = delete;

  Value load(std::memory_order order) const {
    assert(order == std::memory_order_relaxed ||
           order == std::memory_order_acquire ||
           order == std::memory_order_consume);
#if SWIFT_ATOMIC_VECTOR_LOAD16
    if (__builtin_expect(has16ByteAtomicVectorLoads(), true))
      return vectorLoad16<Value>(&atomicValue);
#endif
    // Aligned SSE loads are atomic on every known processor, but
    // the only 16-byte access that's architecturally guaranteed to be
    // atomic is lock cmpxchg16b, so we do that with identical comparison
    // and new values purely for the side-effect of updating the old value.
    __int64 resultArray[2] = {};
#if SWIFT_HAS_MSVC_ARM_ATOMICS
    if (order != std::memory_order_acquire) {
      (void) _InterlockedCompareExchange128_nf(
                            reinterpret_cast<volatile __int64*>(&atomicValue),
                            0, 0, resultArray);
    } else {
#endif
      (void) _InterlockedCompareExchange128(
                            reinterpret_cast<volatile __int64*>(&atomicValue),
                            0, 0, resultArray);
#if SWIFT_HAS_MSVC_ARM_ATOMICS
    }
#endif
    return reinterpret_cast<Value &>(resultArray);
  }

  void store(Value newValue, std::memory_order order) {
    assert(order == std::memory_order_relaxed ||
           order == std::memory_order_release);
    Value oldValue = load(std::memory_order_relaxed);
    while (!compare_exchange_weak(oldValue, newValue,
                                  /*success*/ order,
                                  /*failure*/ std::memory_order_relaxed)) {
      // try again
    }
  }

  bool compare_exchange_weak(Value &oldValue, Value newValue,
                             std::memory_order successOrder,
                             std::memory_order failureOrder) {
    // We do not have weak CAS intrinsics, fallback to strong
    return compare_exchange_strong(oldValue, newValue, successOrder,
                                   failureOrder);
  }

  bool compare_exchange_strong(Value &oldValue, Value newValue,
                               std::memory_order successOrder,
                               std::memory_order failureOrder) {
#if SWIFT_HAS_MSVC_ARM_ATOMICS
    if (successOrder == std::memory_order_relaxed &&
        failureOrder != std::memory_order_acquire) {
      return _InterlockedCompareExchange128_nf(
                            reinterpret_cast<volatile __int64*>(&atomicValue),
                            reinterpret_cast<const __int64*>(&newValue)[1],
                            reinterpret_cast<const __int64*>(&newValue)[0],
                            reinterpret_cast<__int64*>(&oldValue));
    } else if (successOrder == std::memory_order_relaxed) {
      return _InterlockedCompareExchange128_acq(
                            reinterpret_cast<volatile __int64*>(&atomicValue),
                            reinterpret_cast<const __int64*>(&newValue)[1],
                            reinterpret_cast<const __int64*>(&newValue)[0],
                            reinterpret_cast<__int64*>(&oldValue));
    } else if (failureOrder != std::memory_order_acquire) {
      return _InterlockedCompareExchange128_rel(
                            reinterpret_cast<volatile __int64*>(&atomicValue),
                            reinterpret_cast<const __int64*>(&newValue)[1],
                            reinterpret_cast<const __int64*>(&newValue)[0],
                            reinterpret_cast<__int64*>(&oldValue));
    } else {
#endif
      return _InterlockedCompareExchange128(
                            reinterpret_cast<volatile __int64*>(&atomicValue),
                            reinterpret_cast<const __int64*>(&newValue)[1],
                            reinterpret_cast<const __int64*>(&newValue)[0],
                            reinterpret_cast<__int64*>(&oldValue));
#if SWIFT_HAS_MSVC_ARM_ATOMICS
    }
#endif
  }
};

#endif

} // end namespace swift::impl

/// A simple wrapper for std::atomic that provides the most important
/// interfaces and fixes the API bug where all of the orderings default
/// to sequentially-consistent.
///
/// It also sometimes uses a different implementation in cases where
/// std::atomic has made unfortunate choices; our uses of this broadly
/// don't have the ABI-compatibility issues that std::atomic faces.
template <class T>
class atomic : public impl::atomic_impl<T> {
public:
  constexpr atomic(T value) : impl::atomic_impl<T>(value) {}
};

} // end namespace swift

#endif
