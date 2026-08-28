// Check that aggressive CMO gives public linkage to a conformance which is
// declared in the CMO'd module, even if the conforming type is imported from a
// module which was not built with aggressive CMO.
//
// Previously the client either crashed in the LLVM verifier (private protocol)
// or failed to link (internal protocol), because it specialized the serialized
// generic function and then referenced a non-public witness table of the other
// module.
//
// rdar://165092337, https://github.com/swiftlang/swift/issues/91771

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Note: ConformanceTypes is intentionally built *without* -cross-module-optimization.
// RUN: %target-build-swift -O -wmo -parse-as-library -emit-module -emit-module-path=%t/ConformanceTypes.swiftmodule -module-name=ConformanceTypes %t/ConformanceTypes.swift -c -o %t/conformance-types.o
// RUN: %target-build-swift -O -wmo -parse-as-library -cross-module-optimization -emit-module -emit-module-path=%t/ConformanceCore.swiftmodule -module-name=ConformanceCore -I%t %t/ConformanceCore.swift -c -o %t/conformance-core.o

// Check that the witness tables get public linkage in the module which declares
// the conformances.
// RUN: %target-build-swift -O -wmo -parse-as-library -cross-module-optimization -module-name=ConformanceCore -I%t %t/ConformanceCore.swift -emit-sil -o %t/core.sil
// RUN: %FileCheck %s -check-prefix=CHECK-CORE-SIL < %t/core.sil

// CHECK-CORE-SIL-DAG: sil_witness_table Incident.Update: PrivatelyUpdatable module ConformanceCore
// CHECK-CORE-SIL-DAG: sil_witness_table Incident.Report: InternallyUpdatable module ConformanceCore

// RUN: %target-build-swift -O -wmo -module-name=Main -I%t %t/main.swift -c -o %t/main.o
// RUN: %target-swiftc_driver %t/main.o %t/conformance-core.o %t/conformance-types.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// REQUIRES: executable_test

//--- ConformanceTypes.swift

// This module is deliberately *not* built with -cross-module-optimization.
// It only provides the conforming types; the conformances themselves are
// declared in the CMO-enabled module.

public struct Incident {
  public struct Update {
    public var value: Int
    public init(value: Int = 0) { self.value = value }
  }

  public struct Report {
    public var name: String
    public init(name: String = "") { self.name = name }
  }
}

//--- ConformanceCore.swift

// This module is built with -cross-module-optimization. It declares non-public
// protocols and conforms types imported from `ConformanceTypes` to them.
//
// The witness tables and conformance descriptors of those conformances are
// emitted in *this* module, so aggressive CMO must give them public linkage -
// even though the conforming types come from a module which was not built with
// aggressive CMO.

import ConformanceTypes

private protocol PrivatelyUpdatable {
  func writePrivately()
}

extension Incident.Update: PrivatelyUpdatable {
  func writePrivately() { print("private: writing update with value \(value)") }
}

internal protocol InternallyUpdatable {
  func writeInternally()
}

extension Incident.Report: InternallyUpdatable {
  func writeInternally() { print("internal: writing report named \(name)") }
}

public func processPrivately<U>(_ update: U) {
  guard let update = update as? (any PrivatelyUpdatable) else {
    print("private: updates of \(U.self) are not supported")
    return
  }
  update.writePrivately()
}

public func processInternally<U>(_ report: U) {
  guard let report = report as? (any InternallyUpdatable) else {
    print("internal: reports of \(U.self) are not supported")
    return
  }
  report.writeInternally()
}

//--- main.swift

import ConformanceCore
import ConformanceTypes

// CHECK-OUTPUT: private: writing update with value 42
processPrivately(Incident.Update(value: 42))

// CHECK-OUTPUT: internal: writing report named fire
processInternally(Incident.Report(name: "fire"))

// Types which don't conform still take the non-matching path.
// CHECK-OUTPUT: private: updates of Int are not supported
processPrivately(27)

// CHECK-OUTPUT: internal: reports of Int are not supported
processInternally(27)
