/// A serialized SIL body referencing a clang enum case that has since been
/// removed is diagnosed instead of aborting the compiler.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -I %t \
// RUN:   -module-name Lib -o %t/Lib.swiftmodule -swift-version 5

/// Build client fine in the expected behavior.
// RUN: %target-swift-frontend -c -Onone %t/Client.swift -I %t \
// RUN:   -swift-version 5 -o %t/Client.o
// RUN: %target-swift-frontend -c -O %t/Client.swift -I %t \
// RUN:   -swift-version 5 -o %t/Client.o

/// Remove the case `b` referenced from the body of `f`.
// RUN: cp %t/A_Changed.h %t/A.h
// RUN: not %target-swift-frontend -c -Onone %t/Client.swift -I %t \
// RUN:   -swift-version 5 -o %t/Client.o -diagnostic-style llvm 2>&1 \
// RUN:   | %FileCheck %s
// RUN: not %target-swift-frontend -c -O %t/Client.swift -I %t \
// RUN:   -swift-version 5 -o %t/Client.o -diagnostic-style llvm 2>&1 \
// RUN:   | %FileCheck %s

// CHECK: error: reference to declaration 'E.b' broken by a context change; 'E.b' is not found, it was expected to be in 'A'
// CHECK-NOT: *** DESERIALIZATION FAILURE ***

//--- module.modulemap
module A {
  header "A.h"
}

//--- A.h
typedef enum __attribute__((enum_extensibility(closed))) E : int {
  a, b
} E;

//--- A_Changed.h
typedef enum __attribute__((enum_extensibility(closed))) E : int {
  a
} E;

//--- Lib.swift
import A

@_alwaysEmitIntoClient
public func f(_ e: E) -> Int {
  switch e {
  case .a: return 1
  case .b: return 2
  }
}

//--- Client.swift
import Lib
import A

public func g(_ e: E) -> Int { f(e) }
