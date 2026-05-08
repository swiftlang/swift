// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Library module

// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -emit-module-path %t/Library.swiftmodule -emit-sil -o - | %FileCheck -check-prefix LIBRARY-SIL %s
// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -emit-ir -o - | %FileCheck -check-prefix LIBRARY-IR %s

// Application module

// RUN: %target-swift-frontend %t/Application.swift -I %t -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -emit-sil -o - | %FileCheck -check-prefix APPLICATION-SIL %s
// RUN: %target-swift-frontend %t/Application.swift -I %t -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -emit-ir -o - | %FileCheck -check-prefix APPLICATION-IR %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

//--- Library.swift

public protocol Greeter { func greet() }

// @export(interface) struct: FullMetadata should be a strong public
// definition (ExternalLinkage), not linkonce_odr or private.
// LIBRARY-IR: @"$e7Library11ExportedFooVMf" = constant
@export(interface)
public struct ExportedFoo: Greeter {
  public var x: Int
  public init(x: Int) { self.x = x }
  public func greet() { print("foo") }
}

// Non-@export(interface) struct: FullMetadata is lazily emitted per module;
// the Library module should NOT define it eagerly.
// LIBRARY-IR-NOT: @"$e7Library14NonExportedBarVMf"
public struct NonExportedBar: Greeter {
  public var y: Int
  public init(y: Int) { self.y = y }
  public func greet() { print("bar") }
}

// @export(interface) class: FullMetadata should be a strong public definition.
// LIBRARY-IR: @"$e7Library13ExportedClassCMf" = constant
@export(interface)
public class ExportedClass: Greeter {
  public var z: Int
  public init(z: Int) { self.z = z }
  public func greet() { print("class") }
}

// In embedded Swift, protocol witness tables always get shared SIL linkage
// regardless of whether the conforming type has @export(interface). Shared
// linkage lets each importing module emit its own linkonce_odr copy.
// LIBRARY-SIL-DAG: sil_witness_table shared ExportedFoo: Greeter module Library
// LIBRARY-SIL-DAG: sil_witness_table shared NonExportedBar: Greeter module Library
// LIBRARY-SIL-DAG: sil_witness_table shared ExportedClass: Greeter module Library

//--- Application.swift
import Library

// @export(interface) types: Application references their metadata from Library
// as external declarations, not local copies. External declarations appear
// before linkonce_odr definitions in the IR, so check them in IR order.
// APPLICATION-IR: @"$e7Library11ExportedFooVMf" = external global
// APPLICATION-IR: @"$e7Library13ExportedClassCMf" = external global

// Non-@export(interface) struct: Application emits its own linkonce_odr copy.
// APPLICATION-IR: @"$e7Library14NonExportedBarVMf" = linkonce_odr hidden constant

// Witness tables remain shared in the importing module as well, so the
// application emits its own local copy.
// APPLICATION-SIL-DAG: sil_witness_table shared ExportedFoo: Greeter module Library
// APPLICATION-SIL-DAG: sil_witness_table shared NonExportedBar: Greeter module Library
// APPLICATION-SIL-DAG: sil_witness_table shared ExportedClass: Greeter module Library

public func useExportedFoo() -> Any {
  return ExportedFoo(x: 42)
}

public func useNonExportedBar() -> Any {
  return NonExportedBar(y: 99)
}

public func useExportedClass() -> Any {
  return ExportedClass(z: 7)
}

public func useAsGreeter(_ g: any Greeter) { g.greet() }
public func formGreeterFoo() { useAsGreeter(ExportedFoo(x: 1)) }
public func formGreeterBar() { useAsGreeter(NonExportedBar(y: 2)) }
public func formGreeterClass() { useAsGreeter(ExportedClass(z: 3)) }
