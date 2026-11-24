// REQUIRES: CODEGENERATOR=X86
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// On COFF targets, IRGen lazily initializes conformances from other modules,
// so a conformance of a generic type whose associated conformance comes from
// another module gets a witness table pattern instead of a witness table.

// RUN: %swift -target x86_64-unknown-windows-msvc -parse-stdlib -emit-module %t/Other.swift -module-name Other -o %t/Other.swiftmodule
// RUN: %swift -target x86_64-unknown-windows-msvc -parse-stdlib -emit-ir %t/Lib.swift -module-name Lib -I %t -o /dev/null -validate-tbd-against-ir=all

//--- Other.swift
public protocol Q {}
public protocol P { associatedtype A: Q }
public struct X: Q {}

//--- Lib.swift
import Other

public struct G<T>: P {
  public typealias A = X
}
