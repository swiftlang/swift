// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// Library checking
// RUN: %target-swift-frontend -emit-ir -emit-module -o %t/Library.ll %t/Library.swift -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -parse-as-library
// RUN: %FileCheck %s -check-prefix LIBRARY-IR < %t/Library.ll

// Client checking
// RUN: %target-swift-frontend -emit-ir -o %t/Client.ll %t/Client.swift -I %t -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -parse-as-library
// RUN: %FileCheck %s -check-prefix CLIENT-IR < %t/Client.ll

//--- Library.swift

// A stored `var` with `@export(interface)` produces a strongly-defined storage
// global and a strongly-defined unsafe-address accessor.
// LIBRARY-IR-DAG: @"$e7Library15exportedStoredVSivp" = {{(protected )?}}global
// LIBRARY-IR-DAG: define {{(protected |dllexport )?}}swiftcc ptr @"$e7Library15exportedStoredVSivau"()
@export(interface)
public var exportedStoredV: Int = 42

// A stored `let` with `@export(interface)` produces a strongly-defined storage
// constant and a strongly-defined unsafe-address accessor.
// LIBRARY-IR-DAG: @"$e7Library15exportedStoredLSivp" = {{(protected )?}}constant
// LIBRARY-IR-DAG: define {{(protected |dllexport )?}}swiftcc ptr @"$e7Library15exportedStoredLSivau"()
@export(interface)
public let exportedStoredL: Int = 7

// A computed read-only property with `@export(interface)` produces a
// strongly-defined getter (which inherits the model from the var).
// LIBRARY-IR-DAG: define {{(protected |dllexport )?}}swiftcc i64 @"$e7Library18exportedComputedROSivg"()
@export(interface)
public var exportedComputedRO: Int { return 100 }

// A computed read-write property with `@export(interface)` produces strongly
// defined getter and setter (both inherit the model from the var).
// LIBRARY-IR-DAG: define {{(protected |dllexport )?}}swiftcc i64 @"$e7Library18exportedComputedRWSivg"()
// LIBRARY-IR-DAG: define {{(protected |dllexport )?}}swiftcc void @"$e7Library18exportedComputedRWSivs"
@export(interface)
public var exportedComputedRW: Int {
  get { return 200 }
  set { }
}

// `@export(implementation)` defers emission: Library does not provide a
// definition. Each importing client will emit its own `linkonce_odr` copy.
// LIBRARY-IR-NOT: @"$e7Library11implStoredVSivp"
// LIBRARY-IR-NOT: define{{.*}}@"$e7Library11implStoredVSivau"
// LIBRARY-IR-NOT: define{{.*}}@"$e7Library12implComputedSivg"
@export(implementation)
public var implStoredV: Int = 99

@export(implementation)
public var implComputed: Int { return 300 }

// Subscripts: `@export(interface)` propagates to all of the subscript's
// accessors (get / set / modify).
public struct Container {
  public var storage: Int = 0
  public init() {}

  // LIBRARY-IR-DAG: define {{(protected |dllexport )?}}swiftcc i64 @"$e7Library9ContainerVyS2icig"
  // LIBRARY-IR-DAG: define {{(protected |dllexport )?}}swiftcc void @"$e7Library9ContainerVyS2icis"
  // LIBRARY-IR-DAG: define {{(protected |dllexport )?}}swiftcc {{.*}}@"$e7Library9ContainerVyS2iciM"
  @export(interface)
  public subscript(idx: Int) -> Int {
    get { return storage + idx }
    set { storage = newValue - idx }
  }
}

//--- Client.swift
import Library

// The client must reference the @export(interface) accessors externally
// rather than re-emit `linkonce_odr` copies.
// CLIENT-IR-DAG: declare swiftcc ptr @"$e7Library15exportedStoredVSivau"()
// CLIENT-IR-DAG: declare swiftcc ptr @"$e7Library15exportedStoredLSivau"()
// CLIENT-IR-DAG: declare swiftcc i64 @"$e7Library18exportedComputedROSivg"()
// CLIENT-IR-DAG: declare swiftcc i64 @"$e7Library18exportedComputedRWSivg"()
// CLIENT-IR-DAG: declare swiftcc void @"$e7Library18exportedComputedRWSivs"
// CLIENT-IR-DAG: declare swiftcc i64 @"$e7Library9ContainerVyS2icig"

// For @export(implementation), the client emits its own linkonce_odr copy of
// the storage and accessors.
// CLIENT-IR-DAG: @"$e7Library11implStoredVSivp" = linkonce_odr global
// CLIENT-IR-DAG: define linkonce_odr hidden swiftcc ptr @"$e7Library11implStoredVSivau"()
// CLIENT-IR-DAG: define linkonce_odr hidden swiftcc i64 @"$e7Library12implComputedSivg"()

// The client's IR must NOT redefine the @export(interface) storage or
// accessors.
// CLIENT-IR-NOT: @"$e7Library15exportedStoredVSivp" =
// CLIENT-IR-NOT: @"$e7Library15exportedStoredLSivp" =
// CLIENT-IR-NOT: define{{.*}}@"$e7Library15exportedStoredVSivau"
// CLIENT-IR-NOT: define{{.*}}@"$e7Library18exportedComputedROSivg"
// CLIENT-IR-NOT: define{{.*}}@"$e7Library18exportedComputedRWSivs"

public func use() -> Int {
  var c = Container()
  c[0] = exportedStoredV + exportedStoredL + exportedComputedRO
       + exportedComputedRW + implStoredV + implComputed
  exportedComputedRW = c[0]
  return c[0]
}
