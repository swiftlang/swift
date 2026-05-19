// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// Root file checking using explicit @export attributes.
// RUN: %target-swift-frontend -emit-ir -emit-module -o %t/Root.ll %t/Root.swift -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -parse-as-library -DEXPLICIT_EXPORT
// RUN: %FileCheck %s -check-prefix ROOT-IR < %t/Root.ll
// RUN: %target-swift-frontend -emit-sil %t/Root.swiftmodule -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -parse-as-library | %FileCheck %s -check-prefix ROOT-SIL

// Root file checking

// RUN: %target-swift-frontend -emit-ir -emit-module -o %t/Root.ll %t/Root.swift -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -parse-as-library
// RUN: %FileCheck %s -check-prefix ROOT-IR < %t/Root.ll
// RUN: %target-swift-frontend -emit-sil %t/Root.swiftmodule -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -parse-as-library | %FileCheck %s -check-prefix ROOT-SIL

// Client checking
// RUN: %target-swift-frontend -emit-sil -o %t/Client.sil %t/Client.swift -I %t -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -parse-as-library
// RUN: %FileCheck %s -check-prefix CLIENT-SIL < %t/Client.sil
// RUN: %target-swift-frontend -emit-ir -o %t/Client.ll %t/Client.swift -I %t -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -parse-as-library
// RUN: %FileCheck %s -check-prefix CLIENT-IR < %t/Client.ll

//--- Root.swift
struct Point {
  var x, y: Int
}

// Only the specialization, and it's linkonce_odr.
// ROOT-IR-DAG: define linkonce_odr hidden swiftcc ptr @"$e4Root20enumerateByteOffsetsySaySiGxmlFAA5PointV_Ttg5"()
public func enumerateByteOffsets<T>(_: T.Type) -> [Int] {
  var array: [Int] = []
  for i in 0..<MemoryLayout<T>.size {
    array.append(i)
  }
  return array
}

// Strong definition.
// ROOT-IR-DAG: define {{(protected )?}}swiftcc ptr @"$e4Root15getPointOffsetsSaySiGyF"(
// ROOT-SIL: getPointOffsets
// ROOT-SIL-NOT: sil{{.*}}@$e4Root15getPointOffsetsSaySiGyF : $@convention(thin) () -> @owned Array<Int> {
#if EXPLICIT_EXPORT
@export(interface)
#endif
public func getPointOffsets() -> [Int] {
  enumerateByteOffsets(Point.self)
}

// A protocol that types in this module conform to.
public protocol Greeter {
  func greet()
}

// Under CodeGenerationModel=interface, type metadata for non-generic types
// is strongly defined in the Root module (no `linkonce_odr`), and so are
// their protocol witness tables. This holds whether or not @export(interface)
// is written explicitly on the declaration.

// ROOT-IR-DAG: @"$e4Root8MyStructVMf" = {{(protected )?}}constant
// ROOT-IR-DAG: @"$e4Root8MyStructVAA7GreeterAAWP" = {{(protected )?}}constant
// ROOT-SIL-DAG: sil_witness_table MyStruct: Greeter module Root
#if EXPLICIT_EXPORT
@export(interface)
#endif
public struct MyStruct: Greeter {
  public var x: Int
  public init(x: Int) { self.x = x }
  public func greet() { print("MyStruct: \(x)") }
}

// ROOT-IR-DAG: @"$e4Root6MyEnumOMf" = {{(protected )?}}constant
// ROOT-IR-DAG: @"$e4Root6MyEnumOAA7GreeterAAWP" = {{(protected )?}}constant
// ROOT-SIL-DAG: sil_witness_table MyEnum: Greeter module Root
#if EXPLICIT_EXPORT
@export(interface)
#endif
public enum MyEnum: Greeter {
  case a, b, c
  public func greet() { print("MyEnum") }
}

// Classes also expose an address-point alias `…CN` that points into the
// strongly-defined full metadata.
// ROOT-IR-DAG: @"$e4Root7MyClassCMf" = {{(protected )?}}constant
// ROOT-IR-DAG: @"$e4Root7MyClassCN" = {{.*}}alias
// ROOT-IR-DAG: @"$e4Root7MyClassCAA7GreeterAAWP" = {{(protected )?}}constant
// ROOT-SIL-DAG: sil_witness_table MyClass: Greeter module Root
#if EXPLICIT_EXPORT
@export(interface)
#endif
public class MyClass: Greeter {
  public var z: Int
  public init(z: Int) { self.z = z }
  public func greet() { print("MyClass: \(z)") }
}

// A conformance declared in a separate extension still gets a strongly
// defined witness table under CodeGenerationModel=interface, just as if it
// had been declared inline on the type.
// ROOT-IR-DAG: @"$e4Root11PlainStructVMf" = {{(protected )?}}constant
// ROOT-IR-DAG: @"$e4Root11PlainStructVAA7GreeterAAWP" = {{(protected )?}}constant
// ROOT-SIL-DAG: sil_witness_table PlainStruct: Greeter module Root
public struct PlainStruct {
  public var w: Int
  public init(w: Int) { self.w = w }
}

#if EXPLICIT_EXPORT
@export(interface)
#endif
extension PlainStruct: Greeter {
  public func greet() { print("PlainStruct: \(w)") }
}

// Generic types: every concrete instantiation is `linkonce_odr` even when
// the module's CodeGenerationModel is "interface". The unspecialized generic
// has no symbol of its own, and the same specialization may be emitted by
// any module that uses it, so it has to be uniquable rather than strongly
// owned by Root.

// ROOT-IR-DAG: @"$e4Root15MyGenericStructVySiGMf" = linkonce_odr hidden constant
// ROOT-IR-DAG: @"$e4Root15MyGenericStructVySiGAA7GreeterAAWP" = linkonce_odr hidden constant
public struct MyGenericStruct<T> {
  public var v: T
  public init(_ v: T) { self.v = v }
}

extension MyGenericStruct: Greeter {
  public func greet() { print("MyGenericStruct") }
}

// ROOT-IR-DAG: @"$e4Root13MyGenericEnumOySiGMf" = linkonce_odr hidden constant
// ROOT-IR-DAG: @"$e4Root13MyGenericEnumOySiGAA7GreeterAAWP" = linkonce_odr hidden constant
public enum MyGenericEnum<T> {
  case some(T)
  case none
}

extension MyGenericEnum: Greeter {
  public func greet() { print("MyGenericEnum") }
}

// ROOT-IR-DAG: @"$e4Root14MyGenericClassCySiGMf" = linkonce_odr hidden constant
// ROOT-IR-DAG: @"$e4Root14MyGenericClassCySiGAA7GreeterAAWP" = linkonce_odr hidden constant
public class MyGenericClass<T> {
  public var v: T
  public init(_ v: T) { self.v = v }
}

extension MyGenericClass: Greeter {
  public func greet() { print("MyGenericClass") }
}

// Reference all three specializations from Root so their metadata and
// witness tables are emitted into Root's IR.
public func makeIntStruct() -> any Greeter { MyGenericStruct(42) }
public func makeIntEnum() -> any Greeter { MyGenericEnum<Int>.some(1) }
public func makeIntClass() -> any Greeter { MyGenericClass(7) }

//--- Client.swift
import Root

struct MyType { }

// CLIENT-SIL: sil shared @$e4Root20enumerateByteOffsetsySaySiGxmlF6Client6MyTypeV_Ttg5
public func getMyOffsets() -> [Int] {
  enumerateByteOffsets(MyType.self)
}

// The client references Root's type metadata and conformances as external
// declarations rather than re-emitting `linkonce_odr` copies.
// CLIENT-IR-DAG: @"$e4Root8MyStructVMf" = external global
// CLIENT-IR-DAG: @"$e4Root6MyEnumOMf" = external global
// CLIENT-IR-DAG: @"$e4Root7MyClassCMf" = external global
// CLIENT-IR-DAG: @"$e4Root11PlainStructVMf" = external global
// CLIENT-IR-DAG: @"$e4Root8MyStructVAA7GreeterAAWP" = external global
// CLIENT-IR-DAG: @"$e4Root6MyEnumOAA7GreeterAAWP" = external global
// CLIENT-IR-DAG: @"$e4Root7MyClassCAA7GreeterAAWP" = external global
// CLIENT-IR-DAG: @"$e4Root11PlainStructVAA7GreeterAAWP" = external global

// Generic specializations stay `linkonce_odr` in the client too: each
// importing module owns its own copy.
// CLIENT-IR-DAG: @"$e4Root15MyGenericStructVySiGMf" = linkonce_odr hidden constant
// CLIENT-IR-DAG: @"$e4Root13MyGenericEnumOySiGMf" = linkonce_odr hidden constant
// CLIENT-IR-DAG: @"$e4Root14MyGenericClassCySiGMf" = linkonce_odr hidden constant
// CLIENT-IR-DAG: @"$e4Root15MyGenericStructVySiGAA7GreeterAAWP" = linkonce_odr hidden constant
// CLIENT-IR-DAG: @"$e4Root13MyGenericEnumOySiGAA7GreeterAAWP" = linkonce_odr hidden constant
// CLIENT-IR-DAG: @"$e4Root14MyGenericClassCySiGAA7GreeterAAWP" = linkonce_odr hidden constant

// And the client's SIL output does not own the witness tables.
// CLIENT-SIL-NOT: sil_witness_table {{.*}} MyStruct: Greeter
// CLIENT-SIL-NOT: sil_witness_table {{.*}} MyEnum: Greeter
// CLIENT-SIL-NOT: sil_witness_table {{.*}} MyClass: Greeter
// CLIENT-SIL-NOT: sil_witness_table {{.*}} PlainStruct: Greeter

public func useGreeters() {
  let s: any Greeter = MyStruct(x: 1)
  s.greet()
  let e: any Greeter = MyEnum.a
  e.greet()
  let c: any Greeter = MyClass(z: 7)
  c.greet()
  let p: any Greeter = PlainStruct(w: 9)
  p.greet()
  let gs: any Greeter = MyGenericStruct(42)
  gs.greet()
  let ge: any Greeter = MyGenericEnum<Int>.some(1)
  ge.greet()
  let gc: any Greeter = MyGenericClass(7)
  gc.greet()
}
