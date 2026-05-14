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

@export(interface)
public struct ExportedFoo: Greeter {
  public var x: Int
  public init(x: Int) { self.x = x }
  public func greet() { print("foo") }
}

public struct NonExportedBar: Greeter {
  public var y: Int
  public init(y: Int) { self.y = y }
  public func greet() { print("bar") }
}

@export(interface)
public class ExportedClass: Greeter {
  public var z: Int
  public init(z: Int) { self.z = z }
  public func greet() { print("class") }
}

// A plain type with its conformance declared on an @export(interface)
// extension: the extension's attribute makes the conformance
// @export(interface), but does NOT make the type's metadata @export(interface)
// (the type itself has no @export attribute).
public struct PlainStruct {
  public var w: Int
  public init(w: Int) { self.w = w }
}

@export(interface)
extension PlainStruct: Greeter {
  public func greet() { print("plain") }
}

// Library IR:
//  - @export(interface) type metadata: strong public definition (`constant`,
//    or `protected constant` on ELF).
//  - @export(interface) conformances (whether declared on the type itself or
//    on an @export(interface) extension): strong public witness tables
//    (`constant`, or `protected constant` on ELF).
// LIBRARY-IR-DAG: @"$e7Library11ExportedFooVMf" = {{(protected )?}}constant
// LIBRARY-IR-DAG: @"$e7Library13ExportedClassCMf" = {{(protected )?}}constant
// LIBRARY-IR-DAG: @"$e7Library11ExportedFooVAA7GreeterAAWP" = {{(protected )?}}constant
// LIBRARY-IR-DAG: @"$e7Library13ExportedClassCAA7GreeterAAWP" = {{(protected )?}}constant
// LIBRARY-IR-DAG: @"$e7Library11PlainStructVAA7GreeterAAWP" = {{(protected )?}}constant

// Non-@export(interface) type metadata and conformances are NOT eagerly
// emitted in the Library (they stay lazy/shared per importing module).
// PlainStruct's type metadata is also not emitted here (the type itself is
// not @export(interface); only its conformance is).
// LIBRARY-IR-NOT: @"$e7Library14NonExportedBarVMf"
// LIBRARY-IR-NOT: @"$e7Library11PlainStructVMf"
// LIBRARY-IR-NOT: @"$e7Library14NonExportedBarVAA7GreeterAAWP"

// SIL-level linkage:
//  - @export(interface) conformances have "public" SIL linkage (shown with
//    no prefix) so they are owned by this module.
//  - Non-@export(interface) conformances have "shared" SIL linkage so every
//    importing module emits its own linkonce_odr copy.
// LIBRARY-SIL-DAG: sil_witness_table ExportedFoo: Greeter module Library
// LIBRARY-SIL-DAG: sil_witness_table shared NonExportedBar: Greeter module Library
// LIBRARY-SIL-DAG: sil_witness_table ExportedClass: Greeter module Library
// LIBRARY-SIL-DAG: sil_witness_table PlainStruct: Greeter module Library

//--- Application.swift
import Library

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
public func formGreeterPlain() { useAsGreeter(PlainStruct(w: 4)) }

// Application IR:
//  - @export(interface) type metadata: referenced as external declarations.
//  - @export(interface) conformances: referenced as external declarations
//    (the owning module provides the definition).
//  - Non-@export(interface) type metadata: linkonce_odr hidden local copy.
//  - Non-@export(interface) conformances: linkonce_odr hidden local copy.
// APPLICATION-IR-DAG: @"$e7Library11ExportedFooVMf" = external global
// APPLICATION-IR-DAG: @"$e7Library13ExportedClassCMf" = external global
// APPLICATION-IR-DAG: @"$e7Library11ExportedFooVAA7GreeterAAWP" = external global
// APPLICATION-IR-DAG: @"$e7Library13ExportedClassCAA7GreeterAAWP" = external global
// APPLICATION-IR-DAG: @"$e7Library11PlainStructVAA7GreeterAAWP" = external global
// APPLICATION-IR-DAG: @"$e7Library14NonExportedBarVMf" = linkonce_odr hidden constant
// APPLICATION-IR-DAG: @"$e7Library14NonExportedBarVAA7GreeterAAWP" = linkonce_odr hidden constant

// At the SIL level, the application does not own the @export(interface)
// conformances — they aren't added to the SIL witness_table list here. It
// does own a shared copy of the non-@export(interface) conformance.
// APPLICATION-SIL-NOT: sil_witness_table {{.*}} ExportedFoo: Greeter
// APPLICATION-SIL-NOT: sil_witness_table {{.*}} ExportedClass: Greeter
// APPLICATION-SIL-NOT: sil_witness_table {{.*}} PlainStruct: Greeter
// APPLICATION-SIL: sil_witness_table shared NonExportedBar: Greeter module Library
