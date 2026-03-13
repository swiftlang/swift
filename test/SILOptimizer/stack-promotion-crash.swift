// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift -O -wmo -parse-as-library -emit-module -emit-module-path=%t/XMod.swiftmodule -module-name=XMod %t/xmod.swift -I%t -c -o %t/xmod.o
// RUN: %target-build-swift -O -wmo -module-name=Main -I%t %t/main.swift -c -o %t/main.o
// RUN: %target-swiftc_driver %t/main.o %t/xmod.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

//--- module.modulemap

module CModule {
  header "c-header.h"
  export *
}


//--- c-header.h

struct CS {
  int x;
};

//--- xmod.swift

@_implementationOnly import CModule

// The layout of this class does not include the C-imported CS field.
// Therefore it must not be stack-promoted in `testit`. Otherwise the reserved stack space
// would be wrong and the executable would crash.
final public class X {
  public var i: Int
  var cs: CS? = nil

  public init(_ i: Int) { self.i = i }
}

//--- main.swift

import XMod

@inline(never)
func getit(_ x: X, _ y: X) -> Int {
  return x.i + y.i
}

@inline(never)
public func testit() -> Int {
  return getit(X(27), X(11))
}

// CHECK: 38
print(testit())

