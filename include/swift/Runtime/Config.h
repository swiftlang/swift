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

#include "swift/Basic/Compiler.h"
#include "swift/Runtime/CMakeConfig.h"

/// SWIFT_RUNTIME_LIBRARY_VISIBILITY - If a class marked with this attribute is
/// linked into a shared library, then the class should be private to the
/// library and not accessible from outside it.  Can also be used to mark
/// variables and functions, making them private to any shared library they are
/// linked into.
/// On PE/COFF targets, library visibility is the default, so this isn't needed.
#if (__has_attribute(visibility) || SWIFT_GNUC_PREREQ(4, 0, 0)) &&    \
    !defined(__MINGW32__) && !defined(__CYGWIN__) && !defined(_WIN32)
#define SWIFT_RUNTIME_LIBRARY_VISIBILITY __attribute__ ((visibility("hidden")))
#else
#define SWIFT_RUNTIME_LIBRARY_VISIBILITY
#endif

#define SWIFT_RUNTIME_ATTRIBUTE_NOINLINE SWIFT_ATTRIBUTE_NOINLINE
#define SWIFT_RUNTIME_ATTRIBUTE_ALWAYS_INLINE SWIFT_ATTRIBUTE_ALWAYS_INLINE
#define SWIFT_RUNTIME_ATTRIBUTE_NORETURN SWIFT_ATTRIBUTE_NORETURN

/// SWIFT_RUNTIME_BUILTIN_TRAP - On compilers which support it, expands to an expression
/// which causes the program to exit abnormally.
#if __has_builtin(__builtin_trap) || SWIFT_GNUC_PREREQ(4, 3, 0)
# define SWIFT_RUNTIME_BUILTIN_TRAP __builtin_trap()
#elif defined(_MSC_VER)
// The __debugbreak intrinsic is supported by MSVC, does not require forward
// declarations involving platform-specific typedefs (unlike RaiseException),
// results in a call to vectored exception handlers, and encodes to a short
// instruction that still causes the trapping behavior we want.
# define SWIFT_RUNTIME_BUILTIN_TRAP __debugbreak()
#else
# define SWIFT_RUNTIME_BUILTIN_TRAP *(volatile int*)0x11 = 0
#endif

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

// Non-Apple platforms always use 1.
# if !defined(__APPLE__)
#  define SWIFT_CLASS_IS_SWIFT_MASK 1ULL

// Builds for Swift-in-the-OS always use 2.
# elif SWIFT_BNI_OS_BUILD
#  define SWIFT_CLASS_IS_SWIFT_MASK 2ULL

// Builds for Xcode always use 1.
# elif SWIFT_BNI_XCODE_BUILD
#  define SWIFT_CLASS_IS_SWIFT_MASK 1ULL

// Compatibility hook libraries cannot rely on the "is swift" bit being either
// value, since they must work with both OS and Xcode versions of the libraries.
// Generate a reference to a nonexistent symbol so that we get obvious linker
// errors if we try.
# elif SWIFT_COMPATIBILITY_LIBRARY
extern uintptr_t __COMPATIBILITY_LIBRARIES_CANNOT_CHECK_THE_IS_SWIFT_BIT_DIRECTLY__;
#  define SWIFT_CLASS_IS_SWIFT_MASK __COMPATIBILITY_LIBRARIES_CANNOT_CHECK_THE_IS_SWIFT_BIT_DIRECTLY__

// Other builds (such as local builds on developers' computers)
// dynamically choose the bit at runtime based on the current OS
// version.
# else
#  define SWIFT_CLASS_IS_SWIFT_MASK _swift_classIsSwiftMask
#  define SWIFT_CLASS_IS_SWIFT_MASK_GLOBAL_VARIABLE 1
#  define SWIFT_BUILD_HAS_BACK_DEPLOYMENT 1
#  include "BackDeployment.h"

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

// Pointer authentication.
#if __has_feature(ptrauth_calls)
#define SWIFT_PTRAUTH 1
#include <ptrauth.h>
#define __ptrauth_swift_runtime_function_entry \
  __ptrauth(ptrauth_key_function_pointer, 1, \
            SpecialPointerAuthDiscriminators::RuntimeFunctionEntry)
#define __ptrauth_swift_runtime_function_entry_with_key(__key) \
  __ptrauth(ptrauth_key_function_pointer, 1, __key)
#define __ptrauth_swift_runtime_function_entry_strip(__fn) \
  ptrauth_strip(__fn, ptrauth_key_function_pointer)
#define __ptrauth_swift_type_descriptor \
  __ptrauth(ptrauth_key_process_independent_data, 1, \
            SpecialPointerAuthDiscriminators::TypeDescriptor)
#define __ptrauth_swift_dynamic_replacement_key                                \
  __ptrauth(ptrauth_key_process_independent_data, 1,                           \
            SpecialPointerAuthDiscriminators::DynamicReplacementKey)
#define swift_ptrauth_sign_opaque_read_resume_function(__fn, __buffer)         \
  ptrauth_auth_and_resign(__fn, ptrauth_key_function_pointer, 0,               \
                          ptrauth_key_process_independent_code,                \
                          ptrauth_blend_discriminator(__buffer,                \
            SpecialPointerAuthDiscriminators::OpaqueReadResumeFunction))
#define swift_ptrauth_sign_opaque_modify_resume_function(__fn, __buffer)       \
  ptrauth_auth_and_resign(__fn, ptrauth_key_function_pointer, 0,               \
                          ptrauth_key_process_independent_code,                \
                          ptrauth_blend_discriminator(__buffer,                \
            SpecialPointerAuthDiscriminators::OpaqueModifyResumeFunction))
#else
#define SWIFT_PTRAUTH 0
#define __ptrauth_swift_function_pointer(__typekey)
#define __ptrauth_swift_class_method_pointer(__declkey)
#define __ptrauth_swift_protocol_witness_function_pointer(__declkey)
#define __ptrauth_swift_value_witness_function_pointer(__key)
#define __ptrauth_swift_type_metadata_instantiation_function
#define __ptrauth_swift_runtime_function_entry
#define __ptrauth_swift_runtime_function_entry_with_key(__key)
#define __ptrauth_swift_runtime_function_entry_strip(__fn) (__fn)
#define __ptrauth_swift_heap_object_destructor
#define __ptrauth_swift_type_descriptor
#define __ptrauth_swift_dynamic_replacement_key
#define swift_ptrauth_sign_opaque_read_resume_function(__fn, __buffer) (__fn)
#define swift_ptrauth_sign_opaque_modify_resume_function(__fn, __buffer) (__fn)
#endif

#ifdef __cplusplus

/// Copy an address-discriminated signed pointer from the source to the dest.
template <class T>
SWIFT_RUNTIME_ATTRIBUTE_ALWAYS_INLINE
static inline void swift_ptrauth_copy(T *dest, const T *src, unsigned extra) {
#if SWIFT_PTRAUTH
  *dest = ptrauth_auth_and_resign(*src,
                                  ptrauth_key_function_pointer,
                                  ptrauth_blend_discriminator(src, extra),
                                  ptrauth_key_function_pointer,
                                  ptrauth_blend_discriminator(dest, extra));
#else
  *dest = *src;
#endif
}

/// Initialize the destination with an address-discriminated signed pointer.
/// This does not authenticate the source value, so be careful about how
/// you construct it.
template <class T>
SWIFT_RUNTIME_ATTRIBUTE_ALWAYS_INLINE
static inline void swift_ptrauth_init(T *dest, T value, unsigned extra) {
  // FIXME: assert that T is not a function-pointer type?
#if SWIFT_PTRAUTH
  *dest = ptrauth_sign_unauthenticated(value,
                                  ptrauth_key_function_pointer,
                                  ptrauth_blend_discriminator(dest, extra));
#else
  *dest = value;
#endif
}

template <typename T>
SWIFT_RUNTIME_ATTRIBUTE_ALWAYS_INLINE
static inline T swift_auth_data_non_address(T value, unsigned extra) {
#if SWIFT_PTRAUTH
  return (T)ptrauth_auth_data((void *)value,
                               ptrauth_key_process_independent_data,
                               extra);
#else
  return value;
#endif
}

#endif

#endif // SWIFT_RUNTIME_CONFIG_H
