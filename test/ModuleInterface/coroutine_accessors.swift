// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) \
// RUN:     %s \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -module-name Rock
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Rock

// REQUIRES: asserts

var _i: Int = 0

// CHECK:      #if compiler(>=5.3) && $CoroutineAccessors
// CHECK-NEXT: public var i: Swift.Int {
// CHECK-NEXT:   _read
// CHECK-NEXT:   read
// CHECK-NEXT:   _modify
// CHECK-NEXT:   modify
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: public var i: Swift.Int {
// CHECK-NEXT:   _read
// CHECK-NEXT:   _modify
// CHECK-NEXT: }
// CHECK-NEXT: #endif
public var i: Int {
  _read {
    yield _i
  }
  read {
    yield _i
  }
  _modify {
    yield &_i
  }
  modify {
    yield &_i
  }
}
