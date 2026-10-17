// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -clang-header-expose-decls=all-public -emit-clang-header-path %t/empty.h
// RUN: %FileCheck %s --check-prefix=MODULE --implicit-check-not=SWIFT_PRINTED_CORE --implicit-check-not='define SWIFT_TYPEDEFS' --implicit-check-not=_SwiftCxxInteroperability.h < %t/empty.h
// RUN: %target-swift-frontend -parse-as-library %platform-module-dir/Swift.swiftmodule/%module-target-triple.swiftmodule -module-name Swift -parse-stdlib -typecheck -cxx-interoperability-mode=default -clang-header-expose-decls=has-expose-attr-or-stdlib -emit-clang-header-path %t/stdlib.h
// RUN: %FileCheck %s < %t/stdlib.h

// MODULE-LABEL: #ifndef EMPTY_SWIFT_H
// MODULE-NEXT:  #define EMPTY_SWIFT_H
// MODULE:       #include <swift/Swift.h>
// MODULE-LABEL: #if defined(__OBJC__)
// MODULE-NEXT:  #if __has_feature(objc_modules)
// MODULE-LABEL: #if defined(__OBJC__)
// MODULE-EMPTY:
// MODULE-NEXT:  #endif
// MODULE-NEXT:  #if __has_attribute(external_source_symbol)
// MODULE-NEXT:  # pragma clang attribute pop
// MODULE-NEXT:  #endif
// MODULE:       namespace empty SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("empty") {
// MODULE:       } // namespace empty
// MODULE:       #endif
// MODULE-NOT:   @

// Common definitions are emitted once, in the standard library header.
// CHECK-LABEL: #if !defined(__has_include)
// CHECK-NEXT: # define __has_include(x) 0
// CHECK-NEXT: #endif

// CHECK-LABEL: #if !defined(__has_attribute)
// CHECK-NEXT: # define __has_attribute(x) 0
// CHECK-NEXT: #endif

// CHECK-LABEL: #if !defined(__has_feature)
// CHECK-NEXT: # define __has_feature(x) 0
// CHECK-NEXT: #endif

// CHECK-LABEL: #if !defined(__has_warning)
// CHECK-NEXT: # define __has_warning(x) 0
// CHECK-NEXT: #endif

// CHECK-LABEL: #if defined(__OBJC__)
// CHECK-NEXT:  #include <Foundation/Foundation.h>
// CHECK-NEXT:  #endif
// CHECK-NEXT:  #if defined(__cplusplus)
// CHECK-NEXT:  #include <cstdint>
// CHECK-NEXT:  #include <cstddef>
// CHECK-NEXT:  #include <cstring>
// CHECK-NEXT:  #include <stdlib.h>
// CHECK-NEXT:  #include <new>
// CHECK-NEXT:  #include <type_traits>
// CHECK-NEXT:  #else
// CHECK-NEXT:  #include <stdint.h>
// CHECK-NEXT:  #include <stddef.h>
// CHECK-NEXT:  #include <stdbool.h>
// CHECK-NEXT:  #include <string.h>
// CHECK-NEXT:  #endif
// CHECK-NEXT:  #if defined(__cplusplus)
// CHECK-NEXT:  #pragma clang diagnostic push
// CHECK-NEXT:  #pragma clang diagnostic ignored "-Wnon-modular-include-in-framework-module"
// CHECK-NEXT:  #if defined(__arm64e__) && __has_include(<ptrauth.h>)
// CHECK-NEXT:  # include <ptrauth.h>
// CHECK-NEXT:  #else
// CHECK-NEXT:  #pragma clang diagnostic push
// CHECK-NEXT:  #pragma clang diagnostic ignored "-Wreserved-macro-identifier"
// CHECK-NEXT:  # ifndef __ptrauth_swift_value_witness_function_pointer
// CHECK-NEXT:  #  define __ptrauth_swift_value_witness_function_pointer(x)
// CHECK-NEXT:  # endif
// CHECK-NEXT:  # ifndef __ptrauth_swift_class_method_pointer
// CHECK-NEXT:  # define __ptrauth_swift_class_method_pointer(x)
// CHECK-NEXT:  # endif
// CHECK-NEXT:  #pragma clang diagnostic pop
// CHECK-NEXT:  #endif
// CHECK-NEXT:  #pragma clang diagnostic pop
// CHECK-NEXT:  #endif

// CHECK-LABEL: !defined(SWIFT_TYPEDEFS)
// CHECK-NEXT:  # define SWIFT_TYPEDEFS 1
// CHECK:       typedef float swift_float2  __attribute__((__ext_vector_type__(2)));
// CHECK-NEXT:  typedef float swift_float3  __attribute__((__ext_vector_type__(3)));
// CHECK-NEXT:  typedef float swift_float4  __attribute__((__ext_vector_type__(4)));
// CHECK-NEXT:  typedef double swift_double2  __attribute__((__ext_vector_type__(2)));
// CHECK-NEXT:  typedef double swift_double3  __attribute__((__ext_vector_type__(3)));
// CHECK-NEXT:  typedef double swift_double4  __attribute__((__ext_vector_type__(4)));
// CHECK-NEXT:  typedef int swift_int2  __attribute__((__ext_vector_type__(2)));
// CHECK-NEXT:  typedef int swift_int3  __attribute__((__ext_vector_type__(3)));
// CHECK-NEXT:  typedef int swift_int4  __attribute__((__ext_vector_type__(4)));
// CHECK-NEXT:  typedef unsigned int swift_uint2  __attribute__((__ext_vector_type__(2)));
// CHECK-NEXT:  typedef unsigned int swift_uint3  __attribute__((__ext_vector_type__(3)));
// CHECK-NEXT:  typedef unsigned int swift_uint4  __attribute__((__ext_vector_type__(4)));

// CHECK: # define SWIFT_METATYPE(X)
// CHECK: # define SWIFT_CLASS
// CHECK: # define SWIFT_CLASS_NAMED
// CHECK: # define SWIFT_PROTOCOL
// CHECK: # define SWIFT_PROTOCOL_NAMED
// CHECK: # define SWIFT_EXTENSION(M)
// CHECK: # define OBJC_DESIGNATED_INITIALIZER

// CHECK-LABEL: #if defined(__OBJC__)
// CHECK-NEXT:  #if !defined(IBSegueAction)
// CHECK-NEXT:  # define IBSegueAction
// CHECK-NEXT:  #endif

// CHECK-LABEL: # define SWIFT_CALL __attribute__((swiftcall))
// CHECK:       # define SWIFT_INDIRECT_RESULT __attribute__((swift_indirect_result))
// CHECK:       # define SWIFT_CONTEXT __attribute__((swift_context))
// CHECK:       # define SWIFT_ERROR_RESULT __attribute__((swift_error_result))

// CHECK:       #if __has_include(<swiftToCxx/_SwiftCxxInteroperability.h>)
// CHECK-NEXT:  #include <swiftToCxx/_SwiftCxxInteroperability.h>
// CHECK-NEXT:  #elif __has_include(<../../../swift/swiftToCxx/_SwiftCxxInteroperability.h>)
// CHECK-NEXT:  #include <../../../swift/swiftToCxx/_SwiftCxxInteroperability.h>
// CHECK-NEXT:  #elif __has_include(<../../../../../lib/swift/swiftToCxx/_SwiftCxxInteroperability.h>)
// CHECK-NEXT:  #include <../../../../../lib/swift/swiftToCxx/_SwiftCxxInteroperability.h>
// CHECK-NEXT:  #else
// CHECK-NEXT:  #error "Swift C++ interoperability headers are not available"
// CHECK-NEXT:  #endif
// CHECK-NEXT:  #pragma clang diagnostic pop
// CHECK:       #ifndef SWIFT_PRINTED_CORE
// CHECK:       } // namespace swift
// CHECK-EMPTY:
// CHECK-NEXT:  #endif
