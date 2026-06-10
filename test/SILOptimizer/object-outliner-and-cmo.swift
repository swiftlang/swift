// RUN: %empty-directory(%t) 
// RUN: split-file %s %t

// RUN: %target-build-swift -O -wmo -parse-as-library -cross-module-optimization -emit-module -emit-module-path=%t/Singleton.swiftmodule -module-name=Singleton %t/singleton.swift -c -o %t/singleton.o
// RUN: %target-build-swift -O -wmo -parse-as-library -cross-module-optimization -I %t -emit-module -emit-module-path=%t/Shared.swiftmodule -module-name=Shared %t/shared.swift -c -o %t/shared.o
// RUN: %target-build-swift -O -wmo -module-name=Main -I %t %t/main.swift -c -o %t/main.o
// RUN: %target-swiftc_driver %t/main.o %t/shared.o %t/singleton.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

// Don't outline static let objects in external functions.
// This would duplicate singleton objects.

//--- singleton.swift

public final class Singleton: Sendable {
  public static let shared = Singleton()

  nonisolated(unsafe) private var i: Int = 0

  @inline(__always)
  private init() {}

  public func foo() -> Int {
    i += 1
    return i
  }
}

//--- shared.swift

import Singleton

@inline(never)
public func notInlined() -> Int {
  Singleton.shared.foo()
}

public func inlined() -> Int {
  Singleton.shared.foo()
}

//--- main.swift

import Shared

// CHECK:      1
print(Shared.inlined())
// CHECK-NEXT: 2
print(Shared.notInlined())

