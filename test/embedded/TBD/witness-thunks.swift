// Witness thunks are serialized along with the witness tables that refer to
// them, so clients that devirtualize calls through a witness table emit their
// own copies. They aren't strongly defined in their module, even when the
// conforming type is internal, and its witness table is.
//
// FIXME: Cross-module optimization still gives the internal witness that a
// serialized thunk calls public linkage, so this module's TBD file doesn't
// list all of its strong symbols yet.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -o %t/Lib.ll %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface
// RUN: %FileCheck -check-prefix LIBRARY-IR %s < %t/Lib.ll

// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck %s

// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -O
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -O
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: VENDOR=apple

//--- Lib.swift
public protocol P {
  func p() -> Int
}

// LIBRARY-IR-DAG: @"$e3Lib8InternalVAA1PAAWP" = {{(protected )?}}constant
// LIBRARY-IR-DAG: define linkonce_odr {{.*}}@"$e3Lib8InternalVAA1PA2aDP1pSiyFTW"(
struct Internal: P {
  func p() -> Int { 1 }
}

public func makeInternal() -> any P { Internal() }

// Generic code that clients emit uses this type, so they devirtualize calls
// through its witness table.
@usableFromInline
struct Hidden: P {
  @usableFromInline init() {}
  @usableFromInline func p() -> Int { 2 }
}

public func callHidden<T>(_ t: T) -> Int {
  func call<U: P>(_ u: U) -> Int { u.p() }
  return call(Hidden())
}

//--- Client.swift
import Lib

@main
struct Main {
  static func main() {
    print(makeInternal().p())
    print(callHidden(0))
  }
}

// CHECK: 1
// CHECK-NEXT: 2
