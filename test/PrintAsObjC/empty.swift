// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -emit-objc-header-path %t/empty.h
// RUN: %FileCheck %s < %t/empty.h
// RUN: %check-in-clang -std=c99 %t/empty.h
// RUN: %check-in-clang -std=c99 -fno-modules -Qunused-arguments %t/empty.h
// RUN: not %check-in-clang -I %S/Inputs/clang-headers %t/empty.h 2>&1 | %FileCheck %s --check-prefix=CUSTOM-OBJC-PROLOGUE

// Make sure we can handle two bridging headers. rdar://problem/22702104
// RUN: %check-in-clang -include %t/empty.h -std=c99 -fno-modules -Qunused-arguments %t/empty.h

// REQUIRES: objc_interop

// CHECK-NOT: @import Swift;

// CHECK-LABEL: #include <objc/NSObject.h>
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

// CHECK-LABEL: #if defined(__has_feature) && __has_feature(modules)
// CHECK-NEXT: #endif


// CHECK-NOT: {{[@;{}]}}

// CUSTOM-OBJC-PROLOGUE: swift/objc-prologue.h:1:2: error: "Prologue included"
