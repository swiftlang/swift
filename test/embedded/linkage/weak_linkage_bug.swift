// RUN: %empty-directory(%t)
// RUN: split-file %s %t


// RUN: %target-swift-frontend -c -wmo -emit-module -o %t/C.o %t/C.swift -enable-experimental-feature Embedded -parse-as-library -enable-experimental-feature EmbeddedExistentials

// RUN: %target-swift-frontend -num-threads 1 -c -I %t -emit-module -o %t/D.o -o %t/D2.o %t/D.swift %t/D2.swift -wmo -enable-experimental-feature Embedded -parse-as-library -enable-experimental-feature EmbeddedExistentials

// RUN: %target-clang %target-clang-resource-dir-opt %t/C.o %t/D.o %t/D2.o  -o %t/Application
// RUN: %target-run %t/Application | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedExistentials

// CHECK: MyClass.foo

//--- C.swift
public class BaseClass {}

public class MyClass<T>: BaseClass {
  var _t : T
  public init(_ t: T) { self._t = t }
  public func foo() { print("MyClass.foo") }
}

public func foo() {
  let o = MyClass(1)
  o.foo()
}

//--- D.swift
import C

public func bar() -> MyClass<Int>{
  let o = MyClass(5)
  return o
}

@main
struct Main {
  static func main() {
    bar()
    test()
  }
}

//--- D2.swift
import C

public func test() -> MyClass<Int> {
    let r = bar()
    r.foo()
    return r
}
