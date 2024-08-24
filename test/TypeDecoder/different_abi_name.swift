// Tests that reconstructing a type from a mangled name whose type is defined 
// in a separate module which has a different ABI name compared to its regular 
// name works.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: cd %t

// RUN: %target-build-swift -emit-library -emit-module -parse-as-library -module-abi-name Other -g %t/TheModule.swift 
// RUN: %target-build-swift -emit-executable -I %t -L %t -lTheModule -g -o %t/user -emit-module %t/user.swift


// RUN: sed -ne '/\/\/ *DEMANGLE-TYPE: /s/\/\/ *DEMANGLE-TYPE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/user -qualify-types=1 -type-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-TYPE

//--- TheModule.swift
public class Foo {
  let i = 42
  public init() {
  }
}

//--- user.swift

import TheModule

let c = TheModule.Foo()

// DEMANGLE-TYPE: $s5Other3FooCD
// CHECK-TYPE: TheModule.Foo

