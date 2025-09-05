// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(Def)) -module-name Def -emit-module -emit-module-path %t/Def.swiftmodule %S/Inputs/protocol-conformance-redundant-def.swift
// RUN: %target-build-swift-dylib(%t/%target-library-name(Ext)) -module-name Ext -emit-module -emit-module-path %t/Ext.swiftmodule -I%t -L%t -lDef %S/Inputs/protocol-conformance-redundant-ext.swift
// RUN: %target-build-swift -I%t -L%t -lDef -o %t/main %target-rpath(%t) %s
// RUN: %target-codesign %t/main %t/%target-library-name(Def) %t/%target-library-name(Ext)
// RUN: %target-run %t/main %t/%target-library-name(Def) %t/%target-library-name(Ext) 2> >(%FileCheck %s -check-prefix=CHECK-STDERR) | %FileCheck %s

// REQUIRES: executable_test
// XFAIL: OS=windows-msvc

import StdlibUnittest

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif canImport(Android)
import Android
#else
#error("Unsupported platform")
#endif

import Def

let dylibPath = CommandLine.arguments.last!
let openRes = dlopen(dylibPath, RTLD_NOW|RTLD_LOCAL)
assert(openRes != nil, "unable to open extension dylib")

class Sub : Super, Hello {
  func hello() {
    print("Hello")
  }
}

// CHECK-STDERR: Warning: 'main.Sub' conforms to protocol 'Hello', but it also inherits conformance from 'Def.Super'.  Relying on a particular conformance is undefined behaviour.
let s = Sub() as AnyObject as! Hello
// CHECK: Hello
s.hello()

extension GenericStruct: @retroactive Hello where T == String {
  public func hello() {
    print("Hello from main")
  }
}

// CHECK-STDERR: Warning: 'main.GenericSubClass<Swift.String>' conforms to protocol 'Hello', but it also inherits conformance from 'Def.GenericSuperClass<Swift.String>'. Relying on a particular conformance is undefined behaviour.
// CHECK: Hello from main
(GenericStruct<String>() as Any as! Hello).hello()

assert(GenericStruct<Int>() as Any as? Hello == nil)

class GenericSubClass<T>: GenericSuperClass<T> {}
extension GenericSubClass: Hello where T == String {
  func hello() {
    print("Hello from main")
  }
}

// CHECK: Hello from main
(GenericSubClass<String>() as Any as! Hello).hello()

// https://github.com/swiftlang/swift/issues/82889
// CHECK: Expected nil
// CHECK: Expected nil
for _ in 0..<2 {
  if GenericSubClass<Int>() as Any as? Hello != nil {
    print("Unexpected successful cast")
  } else {
    print("Expected nil")
  }
}
