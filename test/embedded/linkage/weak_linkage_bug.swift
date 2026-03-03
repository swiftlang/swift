// RUN: %empty-directory(%t)
// RUN: split-file %s %t


// RUN: %target-swift-frontend -c -wmo -emit-module -o %t/C.o %t/C.swift -enable-experimental-feature Embedded -parse-as-library -enable-experimental-feature EmbeddedExistentials
// RUN: %target-swift-frontend -num-threads 1 -c -I %t -emit-module -o %t/D.o -o %t/D2.o %t/D.swift %t/D2.swift -wmo -enable-experimental-feature Embedded -parse-as-library -enable-experimental-feature EmbeddedExistentials

// RUN: %target-clang %target-clang-resource-dir-opt %t/C.o %t/D.o %t/D2.o  -o %t/Application
// RUN: %target-run %t/Application | %FileCheck %s


// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -wmo -emit-module -o %t/C.ll %t/C.swift -enable-experimental-feature Embedded -parse-as-library -enable-experimental-feature EmbeddedExistentials -enable-experimental-feature CoroutineAccessors
// RUN: %target-swift-frontend -num-threads 1 -emit-ir -I %t -emit-module -o %t/D.ll -o %t/D2.ll %t/D.swift %t/D2.swift -wmo -enable-experimental-feature Embedded -parse-as-library -enable-experimental-feature EmbeddedExistentials -enable-experimental-feature CoroutineAccessors

// RUN:  %FileCheck %s --check-prefix=VTABLE-C < %t/C.ll
// RUN:  %FileCheck %s --check-prefix=VTABLE-D < %t/D.ll


// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedExistentials
// REQUIRES: swift_feature_CoroutineAccessors

// CHECK: MyClass.foo

// Make sure the vtable layouts match.
// VTABLE-C: @"$e1C7MyClassCySiGMf" = linkonce_odr hidden constant <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>
// VTABLE-D: @"$e1C7MyClassCySiGMf" = linkonce_odr hidden constant <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>

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
