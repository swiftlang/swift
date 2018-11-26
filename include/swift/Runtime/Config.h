//===--- Config.h - Swift Language Platform Configuration -------*- C++ -*-===//
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
// Definitions of common interest in Swift.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_CONFIG_H
#define SWIFT_RUNTIME_CONFIG_H

// Bring in visibility attribute macros for library visibility.
#include "llvm/Support/Compiler.h"

/// Does the current Swift platform support "unbridged" interoperation
/// with Objective-C?  If so, the implementations of various types must
/// implicitly handle Objective-C pointers.
///
/// Apple platforms support this by default.
#ifndef SWIFT_OBJC_INTEROP
#ifdef __APPLE__
#define SWIFT_OBJC_INTEROP 1
#else
#define SWIFT_OBJC_INTEROP 0
#endif
#endif

/// Does the current Swift platform allow information other than the
/// class pointer to be stored in the isa field?  If so, when deriving
/// the class pointer of an object, we must apply a
/// dynamically-determined mask to the value loaded from the first
/// field of the object.
///
/// According to the Objective-C ABI, this is true only for 64-bit
/// platforms.
#ifndef SWIFT_HAS_ISA_MASKING
#if SWIFT_OBJC_INTEROP && __POINTER_WIDTH__ == 64
#define SWIFT_HAS_ISA_MASKING 1
#else
#define SWIFT_HAS_ISA_MASKING 0
#endif
#endif

/// Does the current Swift platform have ISA pointers which should be opaque
/// to anyone outside the Swift runtime?  Similarly to the ISA_MASKING case
/// above, information other than the class pointer could be contained in the
/// ISA.
#ifndef SWIFT_HAS_OPAQUE_ISAS
#if defined(__arm__) && __ARM_ARCH_7K__ >= 2
#define SWIFT_HAS_OPAQUE_ISAS 1
#else
#define SWIFT_HAS_OPAQUE_ISAS 0
#endif
#endif

#if SWIFT_HAS_OPAQUE_ISAS && SWIFT_HAS_ISA_MASKING
#error Masking ISAs are incompatible with opaque ISAs
#endif

/// Which bits in the class metadata are used to distinguish Swift classes
/// from ObjC classes?
#ifndef SWIFT_CLASS_IS_SWIFT_MASK
# if defined(__APPLE__) && SWIFT_OBJC_INTEROP && SWIFT_DARWIN_ENABLE_STABLE_ABI_BIT
#  define SWIFT_CLASS_IS_SWIFT_MASK 2ULL
# else
#  define SWIFT_CLASS_IS_SWIFT_MASK 1ULL
# endif
#endif

// We try to avoid global constructors in the runtime as much as possible.
// These macros delimit allowed global ctors.
#if __clang__
# define SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_BEGIN \
    _Pragma("clang diagnostic push") \
    _Pragma("clang diagnostic ignored \"-Wglobal-constructors\"")
# define SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_END \
    _Pragma("clang diagnostic pop")
#else
# define SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_BEGIN
# define SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_END
#endif

// Bring in visibility attribute macros
#include "../../../stdlib/public/SwiftShims/Visibility.h"

// Define mappings for calling conventions.

// Annotation for specifying a calling convention of
// a runtime function. It should be used with declarations
// of runtime functions like this:
// void runtime_function_name() SWIFT_CC(swift)
#define SWIFT_CC(CC) SWIFT_CC_##CC

// SWIFT_CC(c) is the C calling convention.
#define SWIFT_CC_c

// SWIFT_CC(swift) is the Swift calling convention.
// FIXME: the next comment is false.
// Functions outside the stdlib or runtime that include this file may be built 
// with a compiler that doesn't support swiftcall; don't define these macros
// in that case so any incorrect usage is caught.
#if __has_attribute(swiftcall)
#define SWIFT_CC_swift __attribute__((swiftcall))
#define SWIFT_CONTEXT __attribute__((swift_context))
#define SWIFT_ERROR_RESULT __attribute__((swift_error_result))
#define SWIFT_INDIRECT_RESULT __attribute__((swift_indirect_result))
#else
#define SWIFT_CC_swift
#define SWIFT_CONTEXT
#define SWIFT_ERROR_RESULT
#define SWIFT_INDIRECT_RESULT
#endif

// SWIFT_CC(PreserveMost) is used in the runtime implementation to prevent
// register spills on the hot path.
// It is not safe to use for external calls; the loader's lazy function
// binding may not save all of the registers required for this convention.
#if __has_attribute(preserve_most) &&                                          \
    (defined(__aarch64__) || defined(__x86_64__))
#define SWIFT_CC_PreserveMost __attribute__((preserve_most))
#else
#define SWIFT_CC_PreserveMost
#endif

// This is the DefaultCC value used by the compiler.
// FIXME: the runtime's code does not honor DefaultCC
// so changing this value is not sufficient.
#define SWIFT_DEFAULT_LLVM_CC llvm::CallingConv::C

// These are temporary macros during the +0 cc exploration to cleanly support
// both +0 and +1 in the runtime.
#ifndef SWIFT_RUNTIME_ENABLE_GUARANTEED_NORMAL_ARGUMENTS
#define SWIFT_NS_RELEASES_ARGUMENT NS_RELEASES_ARGUMENT
#define SWIFT_CC_PLUSONE_GUARD(...) do { __VA_ARGS__ ; } while (0)
#define SWIFT_CC_PLUSZERO_GUARD(...)
#else
#define SWIFT_NS_RELEASES_ARGUMENT
#define SWIFT_CC_PLUSONE_GUARD(...)
#define SWIFT_CC_PLUSZERO_GUARD(...)  do { __VA_ARGS__ ; } while (0)
#endif

#endif // SWIFT_RUNTIME_CONFIG_H
