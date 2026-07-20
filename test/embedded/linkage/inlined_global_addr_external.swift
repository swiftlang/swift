// Regression test: an inlinable function in a library that references a
// global variable via `global_addr` (e.g. through `Builtin.addressof`) used
// to cause IRGen in the importing client to *redefine* the global. When the
// global is `@export(interface)` — where the library has the unique strong
// definition — the client must instead reference it externally.
//
// `@_silgen_name` on the storage forces direct symbol access, so
// `Builtin.addressof` lowers to `global_addr` rather than calling an
// addressor function. That's the SIL shape that exposed the bug; addressor
// calls hide the global from the importer.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_BuiltinModule
// UNSUPPORTED: CPU=wasm32

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Library.swiftmodule %t/Library.swift -parse-stdlib -enable-experimental-feature BuiltinModule -enable-experimental-feature Embedded -parse-as-library -module-name Library

// RUN: %target-swift-frontend -emit-ir -o %t/Client.ll %t/Client.swift -parse-stdlib -I %t -enable-experimental-feature BuiltinModule -enable-experimental-feature Embedded -parse-as-library -module-name Client
// RUN: %FileCheck %s -check-prefix CLIENT-IR < %t/Client.ll

//--- Library.swift

import Builtin

@export(interface)
@_silgen_name("library_storage")
public var _myStorage: (Builtin.Int64, Builtin.Int64) =
    (Builtin.zeroInitializer(), Builtin.zeroInitializer())

// Inlinable function whose body produces a `global_addr` against the
// library's storage. When this is inlined into a client, the client sees
// the raw address, not an addressor call.
@inlinable
public func getStoragePtr() -> Builtin.RawPointer {
  return Builtin.addressof(&_myStorage)
}

//--- Client.swift
import Builtin
import Library

// The client must reference Library's `library_storage` as an external
// declaration, not redefine it. Before the fix, the client emitted a
// (zero-initialized) definition here, which collided with the library's
// strong definition under the Interface code-generation model.
// CLIENT-IR-DAG: @library_storage = external global
// CLIENT-IR-NOT: @library_storage ={{.*}} global {{.*}}zeroinitializer

public func use() -> Builtin.RawPointer {
  return getStoragePtr()
}
