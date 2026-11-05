// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Lib.swiftinterface) %s -module-name Lib \
// RUN:  
// RUN: %target-swift-typecheck-module-from-interface(%t/Lib.swiftinterface) -module-name Lib \
// RUN:  
// RUN: %FileCheck %s < %t/Lib.swiftinterface


// CHECK: #if compiler(>=5.3) && $Instrumentation
// CHECK-NEXT: @inlinable @instrumentation(disable: address) public func f() {}
@inlinable
@instrumentation(disable: address)
public func f() {}

// CHECK: @inlinable @instrumentation(default, disable: thread, coverage) public func g() {}
@inlinable
@instrumentation(default, disable: thread, coverage)
public func g() {}

// CHECK: @inlinable @instrumentation(disable: address) public subscript(i: Swift::Int) -> Swift::Int {
public struct S {
  @inlinable
  @instrumentation(disable: address)
  public subscript(i: Int) -> Int { i }
}
