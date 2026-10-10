// Cross-module optimization serializes the witness tables of conformances
// that clients can use, along with their witness thunks. The thunks refer to
// witnesses with a unique definition by symbol, so the TBD file lists those
// witnesses, even when they're internal, private, or synthesized. Clients
// can't use conformances to internal protocols, so they aren't serialized.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -o /dev/null %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib -validate-tbd-against-ir=all
// RUN: %FileCheck -check-prefix TBD %s < %t/Lib.tbd
// RUN: %target-swift-frontend -emit-ir -o /dev/null %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib-O.tbd -tbd-install_name Lib -validate-tbd-against-ir=all -O

// RUN: %target-swift-frontend -emit-module -o %t/Lib.swiftmodule %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface
// RUN: %target-sil-opt %t/Lib.swiftmodule -enable-experimental-feature Embedded -emit-sorted-sil | %FileCheck -check-prefix SERIALIZED %s

// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -O
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -O
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: VENDOR=apple

//--- Lib.swift

// Synthesized witnesses of a '@usableFromInline' type.
// TBD-DAG: "_$e3Lib4KindO21__derived_enum_equalsySbAC_ACtFZ"
// TBD-DAG: "_$e3Lib4KindO4hash4intoys6HasherVz_tF"
// TBD-DAG: "_$e3Lib4KindO9hashValueSivg"
// SERIALIZED-DAG: sil_witness_table [serialized] Kind: Equatable module Lib {
@usableFromInline
enum Kind: Hashable { case a, b }

// A hand-written witness of a private type.
// TBD-DAG: "_$e3Lib5Point{{.*}}V11descriptionSSvg"
private struct Point: CustomStringConvertible {
  var x: Int
  var description: String { "(\(x))" }
}

// A witness of a nested type.
// TBD-DAG: "_$e3Lib5OuterV5InnerO21__derived_enum_equalsySbAE_AEtFZ"
@usableFromInline
struct Outer {
  @usableFromInline
  enum Inner: Equatable { case c, d }
}

// Conformances to an internal protocol aren't serialized, so its witnesses
// stay internal.
// TBD-NOT: InternalProto
// SERIALIZED-NOT: sil_witness_table {{.*}}InternalProto
protocol InternalProto { func value() -> Int }
struct ConformsToInternal: InternalProto { func value() -> Int { 5 } }

func usesInternalProto<T: InternalProto>(_ t: T) -> Int { t.value() }

@inline(never)
func describePoint() -> String { Point(x: 7).description }

@usableFromInline
func kinds() -> (Kind, Kind) { (.a, .b) }

@usableFromInline
func inner() -> Outer.Inner { .c }

public func generic<T>(_ t: T) -> Int {
  let (a, b) = kinds()
  var hasher = Hasher()
  a.hash(into: &hasher)
  return (a == b ? 1 : 0) + (inner() == .d ? 1 : 0) + MemoryLayout<T>.size
}

public func nonGeneric() -> Int {
  describePoint().utf8.count + usesInternalProto(ConformsToInternal())
}

//--- Client.swift
import Lib

@main
struct Main {
  static func main() {
    print(generic(Int64(1)))
    print(nonGeneric())
  }
}

// CHECK: 8
// CHECK-NEXT: 8
