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

@export(interface)
public enum ExportedEnum: Greeter, Error, Equatable {
  case red
  case green
  case blue
  public func greet() { print("enum") }
}

public enum NonExportedEnum: Greeter {
  case foo
  case bar
  public func greet() { print("plainEnum") }
}

// A plain type with its conformance declared on an @export(interface)
// extension: the extension's attribute makes the conformance
// @export(interface), but does NOT make the type's metadata @export(interface)
// (the type itself has no @export attribute).
public struct PlainStruct {
  public var w: Int
  public init(w: Int) { self.w = w }
}

// Members of an @export(interface) extension — the conformance witness,
// extra non-witness members, and a computed property's accessor — should
// all inherit @export(interface) from the enclosing extension, even
// though `PlainStruct` itself does not carry @export(interface).
@export(interface)
extension PlainStruct: Greeter {
  public func greet() { print("plain") }
  public func describe() { print("plain w=\(w)") }
  public var doubled: Int { w * 2 }
}

// A second extension on `PlainStruct` *without* @export(interface): its
// member should keep the default per-importer linkonce_odr behavior.
extension PlainStruct {
  public func nonExportedDescribe() { print("plain non-exported w=\(w)") }
}

// A plain enum with `Equatable` conformance declared on an
// @export(interface) extension: the synthesized `__derived_enum_equals`
// is a member of the extension's DeclContext and should inherit
// @export(interface) from it.
public enum PlainEnum {
  case alpha
  case beta
}

@export(interface)
extension PlainEnum: Equatable {
  public func describe() { print("plain enum") }
}

// Library IR:
//  - @export(interface) type metadata: strong public definition (`constant`,
//    or `protected constant` on ELF).
//  - @export(interface) conformances (whether declared on the type itself or
//    on an @export(interface) extension): strong public witness tables
//    (`constant`, or `protected constant` on ELF).
// LIBRARY-IR-DAG: @"$e7Library11ExportedFooVMf" = {{(protected )?}}constant
// LIBRARY-IR-DAG: @"$e7Library13ExportedClassCMf" = {{(protected )?}}constant
// LIBRARY-IR-DAG: @"$e7Library12ExportedEnumOMf" = {{(protected )?}}constant
// LIBRARY-IR-DAG: @"$e7Library11ExportedFooVAA7GreeterAAWP" = {{(protected )?}}constant
// LIBRARY-IR-DAG: @"$e7Library13ExportedClassCAA7GreeterAAWP" = {{(protected )?}}constant
// LIBRARY-IR-DAG: @"$e7Library12ExportedEnumOAA7GreeterAAWP" = {{(protected )?}}constant
// LIBRARY-IR-DAG: @"$e7Library11PlainStructVAA7GreeterAAWP" = {{(protected )?}}constant

// Non-@export(interface) type metadata and conformances are NOT eagerly
// emitted in the Library (they stay lazy/shared per importing module).
// PlainStruct's type metadata is also not emitted here (the type itself is
// not @export(interface); only its conformance is).
// LIBRARY-IR-NOT: @"$e7Library14NonExportedBarVMf"
// LIBRARY-IR-NOT: @"$e7Library15NonExportedEnumOMf"
// LIBRARY-IR-NOT: @"$e7Library11PlainStructVMf"
// LIBRARY-IR-NOT: @"$e7Library14NonExportedBarVAA7GreeterAAWP"
// LIBRARY-IR-NOT: @"$e7Library15NonExportedEnumOAA7GreeterAAWP"

// SIL-level linkage:
//  - @export(interface) conformances have "public" SIL linkage (shown with
//    no prefix) so they are owned by this module.
//  - Non-@export(interface) conformances have "shared" SIL linkage so every
//    importing module emits its own linkonce_odr copy.
// LIBRARY-SIL-DAG: sil_witness_table ExportedFoo: Greeter module Library
// LIBRARY-SIL-DAG: sil_witness_table shared NonExportedBar: Greeter module Library
// LIBRARY-SIL-DAG: sil_witness_table ExportedClass: Greeter module Library
// LIBRARY-SIL-DAG: sil_witness_table ExportedEnum: Greeter module Library
// LIBRARY-SIL-DAG: sil_witness_table shared NonExportedEnum: Greeter module Library
// LIBRARY-SIL-DAG: sil_witness_table PlainStruct: Greeter module Library

// Members of an @export(interface) type or extension inherit the
// `[export_interface]` SIL function attribute, both for user-written
// methods and for compiler-synthesized members (e.g. derived `==`).
// LIBRARY-SIL-DAG: sil [export_interface]{{.*}} @$e7Library11ExportedFooV5greetyyF
// LIBRARY-SIL-DAG: sil [export_interface]{{.*}} @$e7Library13ExportedClassC5greetyyF
// LIBRARY-SIL-DAG: sil [export_interface]{{.*}} @$e7Library12ExportedEnumO5greetyyF
// LIBRARY-SIL-DAG: sil [export_interface]{{.*}} @$e7Library12ExportedEnumO21__derived_enum_equalsySbAC_ACtFZ
// LIBRARY-SIL-DAG: sil [export_interface]{{.*}} @$e7Library11PlainStructV5greetyyF

// Multiple members of a single @export(interface) extension all inherit
// the attribute (the conformance witness, an extra non-witness method,
// and a computed property's getter).
// LIBRARY-SIL-DAG: sil [export_interface]{{.*}} @$e7Library11PlainStructV8describeyyF
// LIBRARY-SIL-DAG: sil [export_interface]{{.*}} @$e7Library11PlainStructV7doubledSivg

// A synthesized `==` from an @export(interface) extension-declared
// `Equatable` conformance also inherits the attribute (the synthesized
// member's DeclContext is the extension itself).
// LIBRARY-SIL-DAG: sil [export_interface]{{.*}} @$e7Library9PlainEnumO21__derived_enum_equalsySbAC_ACtFZ

// And: a non-@export(interface) extension on the same type does NOT
// promote its members. They keep the default shared/linkonce behavior.
// LIBRARY-SIL-NOT: sil [export_interface]{{.*}} @$e7Library11PlainStructV19nonExportedDescribe

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

public func useExportedEnum() -> Any {
  return ExportedEnum.red
}

public func useNonExportedEnum() -> Any {
  return NonExportedEnum.foo
}

public func useAsGreeter(_ g: any Greeter) { g.greet() }
public func formGreeterFoo() { useAsGreeter(ExportedFoo(x: 1)) }
public func formGreeterBar() { useAsGreeter(NonExportedBar(y: 2)) }
public func formGreeterClass() { useAsGreeter(ExportedClass(z: 3)) }
public func formGreeterEnum() { useAsGreeter(ExportedEnum.green) }
public func formGreeterNonExportedEnum() { useAsGreeter(NonExportedEnum.bar) }
public func formGreeterPlain() { useAsGreeter(PlainStruct(w: 4)) }

// Direct (non-existential) calls to the @export(interface) members so
// each appears as an external declaration in the application IR.
public func directExportedFooGreet(_ v: ExportedFoo) { v.greet() }
public func directExportedClassGreet(_ v: ExportedClass) { v.greet() }
public func directExportedEnumGreet(_ v: ExportedEnum) { v.greet() }
public func directPlainStructGreet(_ v: PlainStruct) { v.greet() }

// Force references to all members of the @export(interface) extension
// on PlainStruct (witness, extra method, computed property), the
// non-@export extension's member (negative control), and the
// auto-derived `==` on the @export(interface) extension on PlainEnum.
public func usePlainStructDescribe(_ v: PlainStruct) { v.describe() }
public func usePlainStructDoubled(_ v: PlainStruct) -> Int { v.doubled }
public func useNonExportedDescribe(_ v: PlainStruct) { v.nonExportedDescribe() }
public func comparePlainEnum(_ a: PlainEnum, _ b: PlainEnum) -> Bool { a == b }

// Force a reference to the auto-derived `==` on the @export(interface) enum
// so its synthesized member shows up in the application IR.
public func compareExportedEnum(_ a: ExportedEnum, _ b: ExportedEnum) -> Bool {
  return a == b
}

// Application IR:
//  - @export(interface) type metadata: referenced as external declarations.
//  - @export(interface) conformances: referenced as external declarations
//    (the owning module provides the definition).
//  - Non-@export(interface) type metadata: linkonce_odr hidden local copy.
//  - Non-@export(interface) conformances: linkonce_odr hidden local copy.
// APPLICATION-IR-DAG: @"$e7Library11ExportedFooVMf" = external global
// APPLICATION-IR-DAG: @"$e7Library13ExportedClassCMf" = external global
// APPLICATION-IR-DAG: @"$e7Library12ExportedEnumOMf" = external global
// APPLICATION-IR-DAG: @"$e7Library11ExportedFooVAA7GreeterAAWP" = external global
// APPLICATION-IR-DAG: @"$e7Library13ExportedClassCAA7GreeterAAWP" = external global
// APPLICATION-IR-DAG: @"$e7Library12ExportedEnumOAA7GreeterAAWP" = external global
// APPLICATION-IR-DAG: @"$e7Library11PlainStructVAA7GreeterAAWP" = external global
// APPLICATION-IR-DAG: @"$e7Library14NonExportedBarVMf" = linkonce_odr hidden constant
// APPLICATION-IR-DAG: @"$e7Library15NonExportedEnumOMf" = linkonce_odr hidden constant
// APPLICATION-IR-DAG: @"$e7Library14NonExportedBarVAA7GreeterAAWP" = linkonce_odr hidden constant
// APPLICATION-IR-DAG: @"$e7Library15NonExportedEnumOAA7GreeterAAWP" = linkonce_odr hidden constant

// At the SIL level, the application does not own the @export(interface)
// conformances — they aren't added to the SIL witness_table list here. It
// does own a shared copy of the non-@export(interface) conformance.
// APPLICATION-SIL-NOT: sil_witness_table {{.*}} ExportedFoo: Greeter
// APPLICATION-SIL-NOT: sil_witness_table {{.*}} ExportedClass: Greeter
// APPLICATION-SIL-NOT: sil_witness_table {{.*}} ExportedEnum: Greeter
// APPLICATION-SIL-NOT: sil_witness_table {{.*}} PlainStruct: Greeter
// APPLICATION-SIL-DAG: sil_witness_table shared NonExportedBar: Greeter module Library
// APPLICATION-SIL-DAG: sil_witness_table shared NonExportedEnum: Greeter module Library

// Members of @export(interface) types/extensions are referenced as
// declarations only in the importer (no body emitted here, even for
// synthesized members like derived `==`). Class methods are dispatched
// via the vtable so they don't appear as direct symbol references here.
// APPLICATION-IR-DAG: declare {{.*}}@"$e7Library11ExportedFooV5greetyyF"
// APPLICATION-IR-DAG: declare {{.*}}@"$e7Library12ExportedEnumO5greetyyF"
// APPLICATION-IR-DAG: declare {{.*}}@"$e7Library12ExportedEnumO21__derived_enum_equalsySbAC_ACtFZ"
// APPLICATION-IR-DAG: declare {{.*}}@"$e7Library11PlainStructV5greetyyF"

// All members of the @export(interface) extension on PlainStruct are
// external declarations in the importer (witness, extra method, and the
// computed property's getter).
// APPLICATION-IR-DAG: declare {{.*}}@"$e7Library11PlainStructV8describeyyF"
// APPLICATION-IR-DAG: declare {{.*}}@"$e7Library11PlainStructV7doubledSivg"

// And the synthesized `==` from the @export(interface) extension-declared
// Equatable conformance on PlainEnum.
// APPLICATION-IR-DAG: declare {{.*}}@"$e7Library9PlainEnumO21__derived_enum_equalsySbAC_ACtFZ"

// The non-@export(interface) extension's member is NOT externalized; the
// importer emits its own linkonce_odr copy (i.e. a `define`, not a
// `declare`).
// APPLICATION-IR-DAG: define {{.*}}@"$e7Library11PlainStructV19nonExportedDescribeyyF"
// APPLICATION-IR-NOT: declare {{.*}}@"$e7Library11PlainStructV19nonExportedDescribeyyF"
