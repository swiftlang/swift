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

/// Does the current Swift platform use LLVM's intrinsic "swiftcall"
/// calling convention for Swift functions?
#ifndef SWIFT_USE_SWIFTCALL
#if __has_attribute(swiftcall) || defined(__linux__)
#define SWIFT_USE_SWIFTCALL 1
#else
#define SWIFT_USE_SWIFTCALL 0
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
#if SWIFT_OBJC_INTEROP && defined(__LP64__)
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
#if __ARM_ARCH_7K__ >= 2
#define SWIFT_HAS_OPAQUE_ISAS 1
#else
#define SWIFT_HAS_OPAQUE_ISAS 0
#endif
#endif

#if SWIFT_HAS_OPAQUE_ISAS && SWIFT_HAS_ISA_MASKING
#error Masking ISAs are incompatible with opaque ISAs
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
// void runtime_function_name() SWIFT_CC(RegisterPreservingCC)
#define SWIFT_CC(CC) SWIFT_CC_##CC

#define SWIFT_CC_preserve_most __attribute__((preserve_most))
#define SWIFT_CC_preserve_all  __attribute__((preserve_all))
#define SWIFT_CC_c

#if SWIFT_USE_SWIFTCALL
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

// Map a logical calling convention (e.g. RegisterPreservingCC) to LLVM calling
// convention.
#define SWIFT_LLVM_CC(CC) SWIFT_LLVM_CC_##CC

// Currently, RuntimeFunction.def uses the following calling conventions:
// DefaultCC, RegisterPreservingCC.
// If new runtime calling conventions are added later, they need to be mapped
// here to something appropriate.

// DefaultCC is usually the standard C calling convention.
#define SWIFT_CC_DefaultCC SWIFT_CC_c
#define SWIFT_CC_DefaultCC_IMPL SWIFT_CC_c
#define SWIFT_LLVM_CC_DefaultCC llvm::CallingConv::C

#define SWIFT_LLVM_CC_RegisterPreservingCC llvm::CallingConv::PreserveMost

#if SWIFT_USE_SWIFTCALL
#define SWIFT_LLVM_CC_SwiftCC  llvm::CallingConv::Swift
#else
#define SWIFT_LLVM_CC_SwiftCC  llvm::CallingConv::C
#endif

// If defined, it indicates that runtime function wrappers
// should be used on all platforms, even they do not support
// the new calling convention which requires this.
#define SWIFT_RT_USE_WRAPPERS_ALWAYS 1

// If defined, it indicates that this calling convention is
// supported by the current target.
// TODO: Define it once the runtime calling convention support has
// been integrated into clang and llvm.
#define SWIFT_RT_USE_RegisterPreservingCC 0

#if  __has_attribute(preserve_most)
#define SWIFT_BACKEND_SUPPORTS_RegisterPreservingCC 1
#else
#define SWIFT_BACKEND_SUPPORTS_RegisterPreservingCC 0
#endif


// RegisterPreservingCC is a dedicated runtime calling convention to be used
// when calling the most popular runtime functions.
#if SWIFT_RT_USE_RegisterPreservingCC &&                              \
    SWIFT_BACKEND_SUPPORTS_RegisterPreservingCC && defined(__aarch64__)
// Targets supporting the dedicated runtime convention should use it.
// If a runtime function is using this calling convention, it can
// be invoked only by means of a wrapper, which performs an indirect
// call. Wrappers are generated by the IRGen and added to object files.
// As a result, runtime functions are invoked only indirectly from
// the user code.
// This is a workaround for dynamic linking issues, where a dynamic
// linker may clobber some of the callee-saved registers defined by
// this new calling convention when it performs lazy binding of
// runtime functions using this new calling convention.
#define SWIFT_CC_RegisterPreservingCC                          \
  SWIFT_CC_preserve_most
#define SWIFT_CC_RegisterPreservingCC_IMPL                     \
  SWIFT_CC_preserve_most

// Indicate that wrappers should be used, because it is required
// for the calling convention to get around dynamic linking issues.
#define SWIFT_RT_USE_WRAPPERS 1

#else

// Targets not supporting the dedicated runtime calling convention
// should use the standard calling convention instead.
// No wrappers are required in this case by the calling convention.
#define SWIFT_CC_RegisterPreservingCC SWIFT_CC_c
#define SWIFT_CC_RegisterPreservingCC_IMPL SWIFT_CC_c

#endif

// Bring in visibility attribute macros for library visibility.
#include "llvm/Support/Compiler.h"

// Generates a name of the runtime entry's implementation by
// adding an underscore as a prefix and a suffix.
#define SWIFT_RT_ENTRY_IMPL(Name) _##Name##_

// Library internal way to invoke the implementation of a runtime entry.
// E.g. a runtime function may be called internally via its public API
// or via the function pointer.
#define SWIFT_RT_ENTRY_CALL(Name) Name

// Name of the symbol holding a reference to the
// implementation of a runtime entry.
#define SWIFT_RT_ENTRY_REF(Name) _##Name

// String representation of the symbol's name.
#define SWIFT_RT_ENTRY_REF_AS_STR(Name) "_" #Name

#if defined(SWIFT_RT_USE_WRAPPERS_ALWAYS)
#undef SWIFT_RT_USE_WRAPPERS
#define SWIFT_RT_USE_WRAPPERS
#endif

#if defined(SWIFT_RT_USE_WRAPPERS)

// Both the runtime functions and their implementation are hidden and
// can be directly referenced only inside the runtime library.
// User code can access these runtime entries only indirectly
// via a global function pointer.
// NOTE: In principle, entries may have LLVM_LIBRARY_VISIBILITY,
// because they are never called directly from the code
// produced by IRGen.
// But some of the runtime entries are invoked directly from
// the foundation. Therefore they should be visible.
#define SWIFT_RT_ENTRY_VISIBILITY SWIFT_RUNTIME_EXPORT
#define SWIFT_RT_ENTRY_IMPL_VISIBILITY LLVM_LIBRARY_VISIBILITY

// Prefix of wrappers generated for runtime functions.
#define SWIFT_WRAPPER_PREFIX "swift_rt_"

#else

// Runtime functions are exported, because it should be possible
// to invoke them directly from the user code. But internal
// implementations of runtime functions do not need to be exported.
#define SWIFT_RT_ENTRY_VISIBILITY SWIFT_RUNTIME_EXPORT
#define SWIFT_RT_ENTRY_IMPL_VISIBILITY LLVM_LIBRARY_VISIBILITY

#endif

#if !defined(__USER_LABEL_PREFIX__)
// MSVC doesn't define __USER_LABEL_PREFIX.
#if defined(_MSC_VER)
#define __USER_LABEL_PREFIX__
#else
#error __USER_LABEL_PREFIX__ is undefined
#endif
#endif

// Workaround the bug of clang in Cygwin 64bit
// https://llvm.org/bugs/show_bug.cgi?id=26744
#if defined(__CYGWIN__) && defined(__x86_64__)
#undef __USER_LABEL_PREFIX__
#define __USER_LABEL_PREFIX__
#endif

#define SWIFT_GLUE_EXPANDED(a, b) a##b
#define SWIFT_GLUE(a, b) SWIFT_GLUE_EXPANDED(a, b)
#define SWIFT_SYMBOL_NAME(name) SWIFT_GLUE(__USER_LABEL_PREFIX__, name)

#define SWIFT_QUOTE_EXPANDED(literal) #literal
#define SWIFT_QUOTE(literal) SWIFT_QUOTE_EXPANDED(literal)

#define SWIFT_QUOTED_SYMBOL_NAME(name)                                   \
  SWIFT_QUOTE(SWIFT_SYMBOL_NAME(name))

#endif // SWIFT_RUNTIME_CONFIG_H
