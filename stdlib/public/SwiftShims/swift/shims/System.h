//===--- System.h - Swift ABI system-specific constants ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Here are some fun facts about the target platforms we support!
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_ABI_SYSTEM_H
#define SWIFT_STDLIB_SHIMS_ABI_SYSTEM_H

// In general, these macros are expected to expand to host-independent
// integer constant expressions.  This allows the same data to feed
// both the compiler and runtime implementation.

/******************************* Default Rules ********************************/

/// The least valid pointer value for an actual pointer (as opposed to
/// Objective-C pointers, which may be tagged pointers and are covered
/// separately).  Values up to this are "extra inhabitants" of the
/// pointer representation, and payloaded enum types can take
/// advantage of that as they see fit.
///
/// By default, we assume that there's at least an unmapped page at
/// the bottom of the address space.  4K is a reasonably likely page
/// size.
///
/// The minimum possible value for this macro is 1; we always assume
/// that the null representation is available.
#define SWIFT_ABI_DEFAULT_LEAST_VALID_POINTER 4096

/// The bitmask of spare bits in a function pointer.
#define SWIFT_ABI_DEFAULT_FUNCTION_SPARE_BITS_MASK 0

/// The bitmask of spare bits in a Swift heap object pointer.  A Swift
/// heap object allocation will never set any of these bits.
#define SWIFT_ABI_DEFAULT_SWIFT_SPARE_BITS_MASK 0

/// The bitmask of reserved bits in an Objective-C object pointer.
/// By default we assume the ObjC runtime doesn't use tagged pointers.
#define SWIFT_ABI_DEFAULT_OBJC_RESERVED_BITS_MASK 0

/// The number of low bits in an Objective-C object pointer that
/// are reserved by the Objective-C runtime.
#define SWIFT_ABI_DEFAULT_OBJC_NUM_RESERVED_LOW_BITS 0

/// The ObjC runtime will not use pointer values for which
/// ``pointer & SWIFT_ABI_XXX_OBJC_RESERVED_BITS_MASK == 0 &&
/// pointer & SWIFT_ABI_XXX_SWIFT_SPARE_BITS_MASK != 0``.

// Weak references use a marker to tell when they are controlled by
// the ObjC runtime and when they are controlled by the Swift runtime.
// Non-ObjC platforms don't use this marker.
#define SWIFT_ABI_DEFAULT_OBJC_WEAK_REFERENCE_MARKER_MASK 0
#define SWIFT_ABI_DEFAULT_OBJC_WEAK_REFERENCE_MARKER_VALUE 0

// BridgeObject uses this bit to indicate a tagged value.
#define SWIFT_ABI_DEFAULT_BRIDGEOBJECT_TAG_32 0U
#define SWIFT_ABI_DEFAULT_BRIDGEOBJECT_TAG_64 0x8000000000000000ULL

// Only the bottom 56 bits are used, and heap objects are eight-byte-aligned.
#define SWIFT_ABI_DEFAULT_64BIT_SPARE_BITS_MASK 0xFF00000000000007ULL

// Poison sentinel value recognized by LLDB as a former reference to a
// potentially deinitialized object. It uses no spare bits and cannot point to
// readable memory.
//
// This is not ABI per-se but does stay in-sync with LLDB. If it becomes
// out-of-sync, then users won't see a friendly diagnostic when inspecting
// references past their lifetime.
#define SWIFT_ABI_DEFAULT_REFERENCE_POISON_DEBUG_VALUE_32 0x00000440U
#define SWIFT_ABI_DEFAULT_REFERENCE_POISON_DEBUG_VALUE_64 0x0000000000000440ULL

/*********************************** i386 *************************************/

// Heap objects are pointer-aligned, so the low two bits are unused.
#define SWIFT_ABI_I386_SWIFT_SPARE_BITS_MASK 0x00000003U

// ObjC weak reference discriminator is the LSB.
#define SWIFT_ABI_I386_OBJC_WEAK_REFERENCE_MARKER_MASK  \
  (SWIFT_ABI_DEFAULT_OBJC_RESERVED_BITS_MASK |          \
   1<<SWIFT_ABI_DEFAULT_OBJC_NUM_RESERVED_LOW_BITS)
#define SWIFT_ABI_I386_OBJC_WEAK_REFERENCE_MARKER_VALUE \
  (1<<SWIFT_ABI_DEFAULT_OBJC_NUM_RESERVED_LOW_BITS)

// BridgeObject uses this bit to indicate whether it holds an ObjC object or
// not.
#define SWIFT_ABI_I386_IS_OBJC_BIT 0x00000002U

/*********************************** arm **************************************/

// Heap objects are pointer-aligned, so the low two bits are unused.
#define SWIFT_ABI_ARM_SWIFT_SPARE_BITS_MASK 0x00000003U

// ObjC weak reference discriminator is the LSB.
#define SWIFT_ABI_ARM_OBJC_WEAK_REFERENCE_MARKER_MASK  \
  (SWIFT_ABI_DEFAULT_OBJC_RESERVED_BITS_MASK |          \
   1<<SWIFT_ABI_DEFAULT_OBJC_NUM_RESERVED_LOW_BITS)
#define SWIFT_ABI_ARM_OBJC_WEAK_REFERENCE_MARKER_VALUE \
  (1<<SWIFT_ABI_DEFAULT_OBJC_NUM_RESERVED_LOW_BITS)

// BridgeObject uses this bit to indicate whether it holds an ObjC object or
// not.
#define SWIFT_ABI_ARM_IS_OBJC_BIT 0x00000002U

/*********************************** x86-64 ***********************************/

/// Darwin reserves the low 4GB of address space.
#define SWIFT_ABI_DARWIN_X86_64_LEAST_VALID_POINTER 0x100000000ULL

// Only the bottom 56 bits are used, and heap objects are eight-byte-aligned.
// This is conservative: in practice architectural limitations and other
// compatibility concerns likely constrain the address space to 52 bits.
#define SWIFT_ABI_X86_64_SWIFT_SPARE_BITS_MASK                                 \
  SWIFT_ABI_DEFAULT_64BIT_SPARE_BITS_MASK

// Objective-C reserves the low bit for tagged pointers on macOS, but
// reserves the high bit on simulators.
#define SWIFT_ABI_X86_64_OBJC_RESERVED_BITS_MASK 0x0000000000000001ULL
#define SWIFT_ABI_X86_64_OBJC_NUM_RESERVED_LOW_BITS 1
#define SWIFT_ABI_X86_64_SIMULATOR_OBJC_RESERVED_BITS_MASK 0x8000000000000000ULL
#define SWIFT_ABI_X86_64_SIMULATOR_OBJC_NUM_RESERVED_LOW_BITS 0


// BridgeObject uses this bit to indicate whether it holds an ObjC object or
// not.
#define SWIFT_ABI_X86_64_IS_OBJC_BIT 0x4000000000000000ULL

// ObjC weak reference discriminator is the bit reserved for ObjC tagged
// pointers plus one more low bit.
#define SWIFT_ABI_X86_64_OBJC_WEAK_REFERENCE_MARKER_MASK  \
  (SWIFT_ABI_X86_64_OBJC_RESERVED_BITS_MASK |          \
   1<<SWIFT_ABI_X86_64_OBJC_NUM_RESERVED_LOW_BITS)
#define SWIFT_ABI_X86_64_OBJC_WEAK_REFERENCE_MARKER_VALUE \
  (1<<SWIFT_ABI_X86_64_OBJC_NUM_RESERVED_LOW_BITS)
#define SWIFT_ABI_X86_64_SIMULATOR_OBJC_WEAK_REFERENCE_MARKER_MASK  \
  (SWIFT_ABI_X86_64_SIMULATOR_OBJC_RESERVED_BITS_MASK |          \
   1<<SWIFT_ABI_X86_64_SIMULATOR_OBJC_NUM_RESERVED_LOW_BITS)
#define SWIFT_ABI_X86_64_SIMULATOR_OBJC_WEAK_REFERENCE_MARKER_VALUE \
  (1<<SWIFT_ABI_X86_64_SIMULATOR_OBJC_NUM_RESERVED_LOW_BITS)

/*********************************** arm64 ************************************/

/// Darwin reserves the low 4GB of address space.
#define SWIFT_ABI_DARWIN_ARM64_LEAST_VALID_POINTER 0x100000000ULL

// Android AArch64 reserves the top byte for pointer tagging since Android 11,
// so shift the spare bits tag to the second byte and zero the ObjC tag.
#define SWIFT_ABI_ANDROID_ARM64_SWIFT_SPARE_BITS_MASK 0x00F0000000000007ULL
#define SWIFT_ABI_ANDROID_ARM64_OBJC_RESERVED_BITS_MASK 0x0000000000000000ULL

#if defined(__ANDROID__) && defined(__aarch64__)
#define SWIFT_ABI_ARM64_SWIFT_SPARE_BITS_MASK SWIFT_ABI_ANDROID_ARM64_SWIFT_SPARE_BITS_MASK
#else
// TBI guarantees the top byte of pointers is unused, but ARMv8.5-A
// claims the bottom four bits of that for memory tagging.
// Heap objects are eight-byte aligned.
#define SWIFT_ABI_ARM64_SWIFT_SPARE_BITS_MASK 0xF000000000000007ULL
#endif

// Objective-C reserves just the high bit for tagged pointers.
#define SWIFT_ABI_ARM64_OBJC_RESERVED_BITS_MASK 0x8000000000000000ULL
#define SWIFT_ABI_ARM64_OBJC_NUM_RESERVED_LOW_BITS 0

// BridgeObject uses this bit to indicate whether it holds an ObjC object or
// not.
#define SWIFT_ABI_ARM64_IS_OBJC_BIT 0x4000000000000000ULL

// ObjC weak reference discriminator is the high bit
// reserved for ObjC tagged pointers plus the LSB.
#define SWIFT_ABI_ARM64_OBJC_WEAK_REFERENCE_MARKER_MASK  \
  (SWIFT_ABI_ARM64_OBJC_RESERVED_BITS_MASK |          \
   1<<SWIFT_ABI_ARM64_OBJC_NUM_RESERVED_LOW_BITS)
#define SWIFT_ABI_ARM64_OBJC_WEAK_REFERENCE_MARKER_VALUE \
  (1<<SWIFT_ABI_ARM64_OBJC_NUM_RESERVED_LOW_BITS)

/*********************************** powerpc ********************************/

// Heap objects are pointer-aligned, so the low two bits are unused.
#define SWIFT_ABI_POWERPC_SWIFT_SPARE_BITS_MASK 0x00000003U

/*********************************** powerpc64 ********************************/

// Heap objects are pointer-aligned, so the low three bits are unused.
#define SWIFT_ABI_POWERPC64_SWIFT_SPARE_BITS_MASK                              \
  SWIFT_ABI_DEFAULT_64BIT_SPARE_BITS_MASK

/*********************************** s390x ************************************/

// Top byte of pointers is unused, and heap objects are eight-byte aligned.
// On s390x it is theoretically possible to have high bit set but in practice
// it is unlikely.
#define SWIFT_ABI_S390X_SWIFT_SPARE_BITS_MASK SWIFT_ABI_DEFAULT_64BIT_SPARE_BITS_MASK

// Objective-C reserves just the high bit for tagged pointers.
#define SWIFT_ABI_S390X_OBJC_RESERVED_BITS_MASK 0x8000000000000000ULL
#define SWIFT_ABI_S390X_OBJC_NUM_RESERVED_LOW_BITS 0

// BridgeObject uses this bit to indicate whether it holds an ObjC object or
// not.
#define SWIFT_ABI_S390X_IS_OBJC_BIT 0x4000000000000000ULL

// ObjC weak reference discriminator is the high bit
// reserved for ObjC tagged pointers plus the LSB.
#define SWIFT_ABI_S390X_OBJC_WEAK_REFERENCE_MARKER_MASK  \
  (SWIFT_ABI_S390X_OBJC_RESERVED_BITS_MASK |          \
   1<<SWIFT_ABI_S390X_OBJC_NUM_RESERVED_LOW_BITS)
#define SWIFT_ABI_S390X_OBJC_WEAK_REFERENCE_MARKER_VALUE \
  (1<<SWIFT_ABI_S390X_OBJC_NUM_RESERVED_LOW_BITS)

/*********************************** wasm32 ************************************/

// WebAssembly doesn't reserve low addresses. But without "extra inhabitants" of
// the pointer representation, runtime performance and memory footprint are
// worse. So assume that compiler driver uses wasm-ld and --global-base=1024 to
// reserve low 1KB.
#define SWIFT_ABI_WASM32_LEAST_VALID_POINTER 4096

#endif // SWIFT_STDLIB_SHIMS_ABI_SYSTEM_H
