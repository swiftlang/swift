/// A serialized SIL body referencing a decl from a module that is no longer
/// loaded by clients is diagnosed instead of aborting the compiler.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/C.swift -module-name C \
// RUN:   -o %t/C.swiftmodule -I %t -swift-version 5
// RUN: %target-swift-frontend -emit-module %t/B.swift -module-name B \
// RUN:   -o %t/B.swiftmodule -I %t -swift-version 5
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -module-name Lib \
// RUN:   -o %t/Lib.swiftmodule -I %t -swift-version 5

/// Build client fine in the expected behavior.
// RUN: %target-swift-frontend -c -Onone %t/Client.swift -I %t \
// RUN:   -swift-version 5 -o %t/Client.o
// RUN: %target-swift-frontend -c -O %t/Client.swift -I %t \
// RUN:   -swift-version 5 -o %t/Client.o

/// Rebuild B without re-exporting C, so clients of Lib no longer load C.
// RUN: %target-swift-frontend -emit-module %t/B_Changed.swift -module-name B \
// RUN:   -o %t/B.swiftmodule -I %t -swift-version 5
// RUN: not %target-swift-frontend -c -Onone %t/Client.swift -I %t \
// RUN:   -swift-version 5 -o %t/Client.o -diagnostic-style llvm 2>&1 \
// RUN:   | %FileCheck %s
// RUN: not %target-swift-frontend -c -O %t/Client.swift -I %t \
// RUN:   -swift-version 5 -o %t/Client.o -diagnostic-style llvm 2>&1 \
// RUN:   | %FileCheck %s

// CHECK: error: cannot deserialize the body of '{{.*}}f(_:)' from module 'Lib'; it references module 'C', which is not loaded
// CHECK: note: module 'Lib' may depend on 'C' through an import that clients don't load; import 'C' explicitly to work around this
// CHECK-NOT: *** DESERIALIZATION FAILURE ***

//--- C.swift
public enum CE { case x, y }

//--- B.swift
@_exported import C

//--- B_Changed.swift
public struct Unrelated {}

//--- Lib.swift
import B

@_alwaysEmitIntoClient
public func f(_ i: Int) -> Int {
  let v: CE = i == 0 ? .x : .y
  switch v {
  case .x: return 1
  case .y: return 2
  }
}

//--- Client.swift
import Lib

public func g(_ i: Int) -> Int { f(i) }
