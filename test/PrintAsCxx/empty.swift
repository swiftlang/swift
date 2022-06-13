// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -clang-header-expose-public-decls -emit-clang-header-path %t/empty.h
// RUN: %FileCheck %s < %t/empty.h

// CHECK-LABEL: #ifndef EMPTY_SWIFT_H
// CHECK-NEXT:  #define EMPTY_SWIFT_H

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
// CHECK-NEXT:  #include <cstdbool>
// CHECK-NEXT:  #include <cstring>
// CHECK-NEXT:  #else
// CHECK-NEXT:  #include <stdint.h>
// CHECK-NEXT:  #include <stddef.h>
// CHECK-NEXT:  #include <stdbool.h>
// CHECK-NEXT:  #include <string.h>
// CHECK-NEXT:  #endif
// CHECK-NEXT:  #if __has_include(<ptrauth.h>)
// CHECK-NEXT:  # include <ptrauth.h>
// CHECK-NEXT:  #else
// CHECK-NEXT:  # ifndef __ptrauth_swift_value_witness_function_pointer
// CHECK-NEXT:  #  define __ptrauth_swift_value_witness_function_pointer(x)
// CHECK-NEXT:  # endif
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

// CHECK-LABEL: #if defined(__OBJC__)
// CHECK-NEXT:  #if __has_feature(modules)

// CHECK-LABEL: #if defined(__OBJC__)
// CHECK-NEXT:  #endif
// CHECK-NEXT:  #if defined(__cplusplus)
// CHECK-NEXT:  #ifndef SWIFT_PRINTED_CORE
// CHECK:       } // namespace swift
// CHECK-EMPTY:
// CHECK-NEXT:  #endif
// CHECK:       namespace empty {
// CHECK:       } // namespace empty
// CHECK:       #endif

// CHECK-NOT: @
