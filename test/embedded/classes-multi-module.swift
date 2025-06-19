// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -c -I%t -parse-as-library %t/MyModule.swift -o %t/MyModule.o -emit-module -emit-module-path %t/MyModule.swiftmodule -emit-empty-object-file
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -c -I%t %t/Main.swift -o %t/Main.o
// RUN: %target-clang %t/Main.o %t/MyModule.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

//--- MyModule.swift

public class C<T> {
  public func foo() {
    let x = X<Int>(x: 27)
    x.bar()
  }
}

class D<T>: C<T> {
  override public func foo() {
    print("D")
  }
}

class X<T: BinaryInteger> {
  var x: T

  init(x: T) { self.x = x }
  
  func bar() {
    print(x)
  }
}

class Y<T: BinaryInteger>: X<T> {
  override func bar() {
  }
}

@inline(never)
public func create<T>(_ t: T.Type) -> C<T> {
  return C<T>()
}

//--- Main.swift

import MyModule

@inline(never)
public func testit() {
  let c = create(Int.self)
  c.foo()
}

// CHECK: 27
testit()
