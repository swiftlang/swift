// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -target %module-target-future %t/A.swift -parse-as-library -enable-experimental-feature Embedded -O -emit-module -emit-module-path %t/A.swiftmodule
// RUN: %target-swift-frontend -target %module-target-future %t/B.swift -I %t -module-name B -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -O -emit-module -emit-module-path %t/B.swiftmodule -emit-ir -o - | %FileCheck -check-prefix B %s --implicit-check-not '@"$e1B' --implicit-check-not 'AA5OtherAAWP' --implicit-check-not 'onlyInt{{.*}}Ts5'
// RUN: %target-swift-frontend -target %module-target-future %t/C.swift -I %t -module-name C -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -O -emit-ir | %FileCheck -check-prefix C %s --implicit-check-not 'define {{.*}}@"$e1A'
// RUN: %target-swift-frontend -target %module-target-future %t/C.swift -I %t -module-name C -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -Onone -emit-ir | %FileCheck -check-prefix C-ONONE %s --implicit-check-not 'define {{.*}}@"$e1A{{.*}}Ts5'

// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_ExtendedSpecializedAttribute

//--- A.swift

@inline(never)
public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T { a &+ b }

public struct Box<T> {
  public var v: T
  @inline(never) public init(_ v: T) { self.v = v }
  @inline(never) public func get() -> T { v }
}

public protocol Shape { func area() -> Int }

public struct SBox<T> {
  public var v: T
  @inline(never) public init(_ v: T) { self.v = v }
  @inline(never) public func get() -> T { v }
}
extension SBox: Shape { @inline(never) public func area() -> Int { 1 } }
// Already concrete: A defines its only body.
extension SBox where T == Int { @inline(never) public func onlyInt() -> T { v } }
// Unavailable: no witness table.
public protocol Other {}
@available(*, unavailable) extension SBox: Other {}

public struct Outer<T> {
  public struct Inner<U> {
    public var t: T
  }
}

open class CBox<T> {
  public var v: T
  public init(_ v: T) { self.v = v }
  open func get() -> T { v }
}

//--- B.swift

import A

// B exports 'A.sum<Int>' and 'A.sum<UInt8>', weak, so modules exporting the
// same ones link without errors.
// B-DAG: define weak_odr {{(protected |dllexport )?}}swiftcc {{i(32|64)}} @"$e1A3sumyxx_xts17FixedWidthIntegerRzlFSi_Ts5"
// B-DAG: define weak_odr {{(protected |dllexport )?}}swiftcc {{i8|i32}} @"$e1A3sumyxx_xts17FixedWidthIntegerRzlFs5UInt8V_Ts5"
@specialized(exported: true, where T == Int)
@specialized(exported: true, where T == UInt8)
public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T

// Members, in an extension.
// B-DAG: define weak_odr {{(protected |dllexport )?}}swiftcc {{i(32|64)}} @"$e1A3BoxVyACyxGxcfCSi_Ts5"
// B-DAG: define weak_odr {{(protected |dllexport )?}}swiftcc {{i(32|64)}} @"$e1A3BoxV3getxyFSi_Ts5"
extension Box {
  @specialized(exported: true, where T == Int)
  public init(_ v: T)

  @specialized(exported: true, where T == Int)
  public func get() -> T
}

// B owns the specializations of A's types: metadata, value witness tables,
// witness tables and members, weak.
@specialized(exported: true, where T == Int)
@specialized(exported: true, where T == UInt8)
extension SBox {}

@specialized(exported: true, where T == Int, U == Int)
extension Outer.Inner {}

@specialized(exported: true, where T == Int)
extension CBox {}

// B also owns the witness tables of its own conformances of the type.
// B-DAG: @"$e1A4SBoxVySiG1B1QAEWP" = weak_odr {{(protected |dllexport )?}}{{(local_unnamed_addr )?}}constant
public protocol Q { func q() -> Int }
extension SBox: Q { public func q() -> Int { 2 } }

// B-DAG: @"$e1A4SBoxVySiGMf" = weak_odr {{(protected |dllexport )?}}{{(local_unnamed_addr )?}}constant
// B-DAG: @"$e1A4SBoxVySiGWV" = weak_odr {{(protected |dllexport )?}}{{(local_unnamed_addr )?}}constant
// B-DAG: @"$e1A4SBoxVySiGAA5ShapeAAWP" = weak_odr {{(protected |dllexport )?}}{{(local_unnamed_addr )?}}constant
// The vtable and deinit are the exported members.
// B-DAG: @"$e1A4CBoxCySiGMf" = weak_odr {{(protected |dllexport )?}}{{(local_unnamed_addr )?}}constant {{.*}} ptr @"$e1A4CBoxCfDSi_Ts5{{(\.ptrauth)?}}", ptr null, ptr @"$e1A4CBoxC1vxvgSi_Ts5{{(\.ptrauth)?}}", ptr @"$e1A4CBoxC1vxvsSi_Ts5{{(\.ptrauth)?}}", ptr @"$e1A4CBoxC1vxvMSi_Ts5{{(\.ptrauth)?}}", ptr @"$e1A4CBoxCyACyxGxcfCSi_Ts5{{(\.ptrauth)?}}", ptr @"$e1A4CBoxC3getxyFSi_Ts5{{(\.ptrauth)?}}" }>

// A second attribute, and a nested type.
// B-DAG: @"$e1A4SBoxVys5UInt8VGMf" = weak_odr {{(protected |dllexport )?}}{{(local_unnamed_addr )?}}constant
// B-DAG: @"$e1A5OuterV5InnerVySi_SiGMf" = weak_odr {{(protected |dllexport )?}}{{(local_unnamed_addr )?}}constant

// Members, for every entry point.
// B-DAG: define weak_odr {{(protected |dllexport )?}}swiftcc {{.*}}@"$e1A4SBoxVyACyxGxcfCSi_Ts5"
// B-DAG: define weak_odr {{(protected |dllexport )?}}swiftcc {{.*}}@"$e1A4SBoxV4areaSiyFSi_Ts5"
// B-DAG: define weak_odr {{(protected |dllexport )?}}swiftcc {{.*}}@"$e1A4CBoxCyACyxGxcfcSi_Ts5"
// B-DAG: define weak_odr {{(protected |dllexport )?}}swiftcc {{.*}}@"$e1A4CBoxCfdSi_Ts5"

//--- C.swift

import A
import B

// C uses B's metadata, witness tables and bodies, with no copies of its own.
// C-DAG: @"$e1A4SBoxVySiGMf" = external global
// C-ONONE-DAG: @"$e1A4SBoxVySiGMf" = external global
// C-ONONE-DAG: @"$e1A4CBoxCySiGMf" = external global
// C-DAG: @"$e1A4SBoxVySiGAA5ShapeAAWP" = external global
// C-DAG: @"$e1A4CBoxCySiGMf" = external global

// A subclass's superclass and inherited vtable entries are B's.
// C-DAG: @"$e1C3SubCMf" = {{.*}} ptr @"$e1A4CBoxCySiGMf"{{.*}} ptr @"$e1A4CBoxC1vxvgSi_Ts5{{(\.ptrauth)?}}"{{.*}} ptr @"$e1A4CBoxC3getxyFSi_Ts5{{(\.ptrauth)?}}" }>
final class Sub: CBox<Int> {}

public func get(_ b: SBox<Int>) -> Int { b.get() }
public func shape(_ b: SBox<Int>) -> any Shape { b }
// C-DAG: @"$e1A4SBoxVySiG1B1QAEWP" = external global
public func q(_ b: SBox<Int>) -> any Q { b }
public func make() -> [Any] { [Sub(2)] }

// Also at -Onone, which doesn't optimize B's deserialized bodies into its own.
// C-ONONE: call swiftcc {{i(32|64)}} @"$e1A3sumyxx_xts17FixedWidthIntegerRzlFSi_Ts5"
// C-LABEL: define {{.*}}@"$e1C3useyS2iF"
// C: call swiftcc {{i(32|64)}} @"$e1A3sumyxx_xts17FixedWidthIntegerRzlFSi_Ts5"
public func use(_ x: Int) -> Int { sum(x, x) }

// C-LABEL: define {{.*}}@"$e1C6useBox{{[^"]*}}"
// C: call swiftcc {{i(32|64)}} @"$e1A3BoxV3getxyFSi_Ts5"
public func useBox(_ b: Box<Int>) -> Int { b.get() }
