// RUN: %target-swift-frontend -emit-interface-path %t.swiftinterface -enable-resilience -emit-module -o /dev/null %s
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: public func hasSimpleDefaultArgs(_ x: Int = 0, b: Int = 1){{$}}
public func hasSimpleDefaultArgs(_ x: Int = 0, b: Int = 1) {
}

// CHECK: public func hasClosureDefaultArg(_ x: () -> Void = {{{$}}
// CHECK-NEXT: }){{$}}
public func hasClosureDefaultArg(_ x: () -> Void = {
}) {
}

// CHECK: public func hasClosureDefaultArgWithSinglePoundIf(_ x: () -> Void = {{{$}}
// CHECK-NOT: #if true
// CHECK: print("true")
// CHECK-NOT: #else
// CHECK-NOT: print("false")
// CHECK-NOT: #endif
// CHECK-NEXT: }){{$}}
public func hasClosureDefaultArgWithSinglePoundIf(_ x: () -> Void = {
  #if true
  print("true")
  #else
  print("false")
  #endif
}) {
}

// CHECK: public func hasClosureDefaultArgWithComplexPoundIf(_ x: () -> Void = {{{$}}
// CHECK-NOT: #if NOT_PROVIDED
// CHECK-NOT: print("should not exist")
// CHECK-NOT: #else
// CHECK-NOT: #if NOT_PROVIDED
// CHECK-NOT: print("should also not exist")
// CHECK-NOT: #else
// CHECK: print("should exist"){{$}}
// CHECK-NOT: #if !second
// CHECK: print("should also exist"){{$}}
// CHECK-NOT: #endif
// CHECK-NEXT: }){{$}}
public func hasClosureDefaultArgWithComplexPoundIf(_ x: () -> Void = {
  #if NOT_PROVIDED
    print("should not exist")
    #else
      #if NOT_PROVIDED
    print("should also not exist")
      #else
    print("should exist")
      #endif
    #endif

    #if !second
    print("should also exist")
    #endif
}) {
}

