// Tests that reconstructing from mangled names whose types are defined in modules
// with clashing ABI names works.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: cd %t

// RUN: %target-build-swift -emit-library -emit-module -parse-as-library -module-name Foo -module-abi-name Bar -g %t/Foo.swift 
// RUN: %target-build-swift -emit-executable -I %t -L %t -lFoo -g -emit-module -module-name Bar -module-abi-name Bar %t/Bar.swift


// RUN: sed -ne '/\/\/ *DEMANGLE-TYPE: /s/\/\/ *DEMANGLE-TYPE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/Bar -qualify-types=1 -type-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-TYPE

//--- Foo.swift
public class One {
  let i = 42
  public init() {
  }
}

public class Generic<T> {
  let t: T
  public init(t: T) {
    self.t = t
  }
}

//--- Bar.swift
import Foo

public class Two {
  let j = 98
  public init() {
  }
}

public class Generic2<T> {
  let t: T
  public init(t: T) {
    self.t = t
  }
}


let one = Foo.One()
let two = Bar.Two()
let generic1 = Foo.Generic<Bar.Two>(t: two)
let generic2 = Bar.Generic2<Foo.Generic<Bar.Two>>(t: generic1)
let generic3 = Foo.Generic<Bar.Generic2<Foo.Generic<Bar.Two>>>(t: generic2)

// DEMANGLE-TYPE: $s3Bar3OneCD
// CHECK-TYPE: Foo.One

// DEMANGLE-TYPE: $s3Bar3TwoCD
// CHECK-TYPE: Bar.Two

// DEMANGLE-TYPE: $s3Bar7GenericCyAA3TwoCGD
// CHECK-TYPE: Foo.Generic<Bar.Two>

// DEMANGLE-TYPE: $s3Bar8Generic2CyAA7GenericCyAA3TwoCGGD
// CHECK-TYPE: Bar.Generic2<Foo.Generic<Bar.Two>>

// DEMANGLE-TYPE: $s3Bar7GenericCyAA8Generic2CyACyAA3TwoCGGGD
// CHECK-TYPE: Foo.Generic<Bar.Generic2<Foo.Generic<Bar.Two>>>
