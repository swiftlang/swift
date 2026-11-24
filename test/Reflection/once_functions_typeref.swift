// Verify that swift-reflection-dump distinguishes at-most-once and exactly-once
// function types from other function types.

// REQUIRES: no_asan
// REQUIRES: swift_feature_OnceFunctions

// LC_DYLD_CHAINED_FIXUPS decode not currently supported (default on visionOS)
// UNSUPPORTED: OS=xros

// rdar://100805115
// UNSUPPORTED: CPU=arm64e

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-future-triple %s -parse-as-library -emit-module -emit-library -module-name CalledFunctions -enable-experimental-feature OnceFunctions -o %t/%target-library-name(CalledFunctions)
// RUN: %target-swift-reflection-dump %t/%target-library-name(CalledFunctions) | %FileCheck %s

struct Holder: ~Copyable {
  var plain: () -> Void
  var atMostOnce: @callable(once) (Int) -> Void
}

struct Consumers {
  var takesAtMostOnce: (@escaping @callable(once) () -> Void) -> Void
  var takesExactlyOnce: (@escaping @once () -> Void) -> Void
}

// CHECK: CalledFunctions.Holder
// CHECK: plain: () -> ()
// CHECK-NEXT: (function
// CHECK-NEXT:   (parameters)
// CHECK-NEXT:   (result
// CHECK-NEXT:     (tuple))

// CHECK: atMostOnce: @callable(once) (Swift.Int) -> ()
// CHECK-NEXT: (function execution-semantics=at-most-once
// CHECK-NEXT:   (parameters
// CHECK-NEXT:     (struct Swift.Int)
// CHECK-NEXT:   (result
// CHECK-NEXT:     (tuple))

// CHECK: CalledFunctions.Consumers
// CHECK: takesAtMostOnce: (__owned @callable(once) () -> ()) -> ()
// CHECK-NEXT: (function
// CHECK-NEXT:   (parameters
// CHECK-NEXT:     (owned
// CHECK-NEXT:       (function execution-semantics=at-most-once
// CHECK-NEXT:         (parameters)
// CHECK-NEXT:         (result
// CHECK-NEXT:           (tuple)))
// CHECK-NEXT:   (result
// CHECK-NEXT:     (tuple))

// CHECK: takesExactlyOnce: (__owned @once () -> ()) -> ()
// CHECK-NEXT: (function
// CHECK-NEXT:   (parameters
// CHECK-NEXT:     (owned
// CHECK-NEXT:       (function execution-semantics=exactly-once
// CHECK-NEXT:         (parameters)
// CHECK-NEXT:         (result
// CHECK-NEXT:           (tuple)))
// CHECK-NEXT:   (result
// CHECK-NEXT:     (tuple))
