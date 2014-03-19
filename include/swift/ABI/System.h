//===--- System.h - Swift ABI system-specific constants ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Here's some fun facts about the target platforms we support!
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_SYSTEM_H
#define SWIFT_ABI_SYSTEM_H

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

/// The bitmask of spare bits in a Swift heap object pointer.  A Swift
/// heap object allocation will never set any of these bits.
#define SWIFT_ABI_DEFAULT_SWIFT_SPARE_BITS_MASK 0

/// The bitmask of spare bits in an Objective-C object pointer.  An
/// Objective-C object value will never set any of these bits.  Note
/// that some platforms support tagged pointers and therefore this set
/// can be a subset of SwiftSpareBitsMask.
#define SWIFT_ABI_DEFAULT_OBJC_SPARE_BITS_MASK 0

/// The bitmask of reserved bits in an Objective-C object pointer.
#define SWIFT_ABI_DEFAULT_OBJC_RESERVED_BITS_MASK 0

/// The number of low bits in an Objective-C object pointer that
/// are reserved by the Objective-C runtime.
#define SWIFT_ABI_DEFAULT_OBJC_NUM_RESERVED_LOW_BITS 0


/*********************************** x86-64 ***********************************/

/// Darwin reserves the low 4GB of address space.
#define SWIFT_ABI_DARWIN_X86_64_LEAST_VALID_POINTER (4ULL*1024*1024*1024)

// Only the bottom 47 bits are used, and heap objects are eight-byte-aligned.
#define SWIFT_ABI_X86_64_SWIFT_SPARE_BITS_MASK 0xFFFF800000000007ULL

// Objective-C reserves the high and low bits for tagged pointers.
#define SWIFT_ABI_X86_64_OBJC_SPARE_BITS_MASK 0x8FFF800000000006ULL
#define SWIFT_ABI_X86_64_OBJC_RESERVED_BITS_MASK 0x8000000000000001ULL
#define SWIFT_ABI_X86_64_OBJC_NUM_RESERVED_LOW_BITS 1


/*********************************** arm64 ************************************/

/// Darwin reserves the low 4GB of address space.
#define SWIFT_ABI_DARWIN_ARM64_LEAST_VALID_POINTER (4ULL*1024*1024*1024)

// TBI guarantees the top byte of pointers is unused.
// Heap objects are eight-byte aligned.
#define SWIFT_ABI_ARM64_SWIFT_SPARE_BITS_MASK 0xFF00000000000007ULL

// Objective-C reserves the high and low bits for tagged pointers.
#define SWIFT_ABI_ARM64_OBJC_SPARE_BITS_MASK 0x8F00000000000006ULL
#define SWIFT_ABI_ARM64_OBJC_RESERVED_BITS_MASK 0x8000000000000001ULL
#define SWIFT_ABI_ARM64_OBJC_NUM_RESERVED_LOW_BITS 1


#endif /* SWIFT_ABI_SYSTEM_H */
