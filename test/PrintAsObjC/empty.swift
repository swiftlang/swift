// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -verify -emit-objc-header-path %t/empty.h
// RUN: %FileCheck %s < %t/empty.h

// RUN: %check-in-clang -std=c99 %t/empty.h
// RUN: %check-in-clang -std=c11 %t/empty.h
// RUN: %check-cxx-header-in-clang -x objective-c++-header -std=c++98 -D_LIBCPP_CSTDLIB %t/empty.h
// RUN: %check-cxx-header-in-clang -x objective-c++-header -std=c++11 -D_LIBCPP_CSTDLIB %t/empty.h
// RUN: %check-cxx-header-in-clang -x objective-c++-header -std=c++14 -D_LIBCPP_CSTDLIB %t/empty.h

// RUN: %check-in-clang -std=c99 -fno-modules -Qunused-arguments %t/empty.h
// RUN: not %check-in-clang -I %S/Inputs/clang-headers %t/empty.h 2>&1 | %FileCheck %s --check-prefix=CUSTOM-OBJC-PROLOGUE

// Make sure we can handle two bridging headers. rdar://problem/22702104
// RUN: %check-in-clang -include %t/empty.h -std=c99 -fno-modules -Qunused-arguments %t/empty.h

// REQUIRES: objc_interop

// CHECK-NOT: @import Swift;

// CHECK-LABEL: #if !defined(__has_feature)
// CHECK-NEXT: # define __has_feature(x) 0
// CHECK-NEXT: #endif

// CHECK-LABEL: #include <Foundation/Foundation.h>
// CHECK: #include <stdint.h>
// CHECK: #include <stddef.h>
// CHECK: #include <stdbool.h>

// CHECK: # define SWIFT_METATYPE(X)
// CHECK: # define SWIFT_CLASS
// CHECK: # define SWIFT_CLASS_NAMED
// CHECK: # define SWIFT_PROTOCOL
// CHECK: # define SWIFT_PROTOCOL_NAMED
// CHECK: # define SWIFT_EXTENSION(M)
// CHECK: # define OBJC_DESIGNATED_INITIALIZER

// CHECK-LABEL: #if __has_feature(objc_modules)
// CHECK-NEXT: #if __has_warning
// CHECK-NEXT: #pragma clang diagnostic
// CHECK-NEXT: #endif
// CHECK-NEXT: #endif


// CHECK-NOT: {{[@;{}]}}

// CUSTOM-OBJC-PROLOGUE: swift/objc-prologue.h:1:2: error: "Prologue included"
