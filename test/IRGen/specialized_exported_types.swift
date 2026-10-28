// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The library owns the metadata, value witness tables, protocol witness
// tables and members of its exported type specializations, even unused ones.
// RUN: %target-swift-frontend -target %module-target-future %t/Library.swift -parse-as-library -module-name Library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -O -emit-ir -o %t/Library-O.ll
// RUN: %FileCheck -check-prefix LIBRARY %s < %t/Library-O.ll
// RUN: %FileCheck -check-prefix LIBRARY-NOT %s < %t/Library-O.ll
// RUN: %FileCheck -check-prefix LIBRARY-O-NOT %s < %t/Library-O.ll
// RUN: %target-swift-frontend -target %module-target-future %t/Library.swift -parse-as-library -module-name Library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -Onone -emit-ir -o %t/Library-Onone.ll
// RUN: %FileCheck -check-prefix LIBRARY %s < %t/Library-Onone.ll
// RUN: %FileCheck -check-prefix LIBRARY-NOT %s < %t/Library-Onone.ll

// Clients reference them.
// RUN: %target-swift-frontend -target %module-target-future %t/Library.swift -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -O -emit-module -emit-module-path %t/Modules/Library.swiftmodule
// RUN: %target-swift-frontend -target %module-target-future %t/Application.swift -I %t/Modules -module-name Application -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -O -emit-ir -o %t/Application-O.ll
// RUN: %FileCheck -check-prefix APPLICATION %s < %t/Application-O.ll
// RUN: %FileCheck -check-prefix APPLICATION-NOT %s < %t/Application-O.ll

// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_ExtendedSpecializedAttribute

//--- Library.swift

public protocol Shape { func area() -> Int; func scaled<U>(_ u: U) -> Int }
public protocol Text { func text() -> Int }

@specialized(exported: true, where T == Int)
public struct SBox<T> {
  public var v: T
  public init(_ v: T) { self.v = v }
  public func map<U>(_ f: (T) -> U) -> U { f(v) }
  // A member's own attributes add to the type's.
  @specialized(exported: true, where T == UInt8)
  public func other() -> T { v }
  // Code emitted into clients and internal members aren't the library's.
  @_alwaysEmitIntoClient public func aeic() -> T { v }
  @_alwaysEmitIntoClient public var computed: T { v }
  @export(implementation) public func impl() -> T { v }
  @_transparent public func transparent() -> T { v }
  @_transparent public var transparentVar: T { v }
  public func defaultArgument(_ x: Int = 0) -> T { v }
  func hidden() -> T { v }
}
@export(implementation) extension SBox: Equatable where T: Equatable {}
extension SBox where T: Numeric { public func doubled() -> T { v + v } }
extension SBox where T: StringProtocol { public func upper() -> Int { 1 } }
extension SBox: Shape {
  public func area() -> Int { 1 }
  public func scaled<U>(_ u: U) -> Int { 2 }
}
extension SBox: Text where T: StringProtocol { public func text() -> Int { 3 } }
// Marker protocols have no witness table.
extension SBox: Sendable where T: Sendable {}
protocol Hidden {}
extension SBox: Hidden {}

@specialized(exported: true, where T == Int)
public enum EBox<T> { case a(T), b }

@specialized(exported: true, where T == Int)
open class CBox<T> {
  public var v: T
  public init(_ v: T) { self.v = v }
  open func get() -> T { v }
}

// Metadata and value witness tables.
// LIBRARY-DAG: @"$e7Library4SBoxVySiGMf" = {{(dso_local |dllexport |protected )?}}{{(local_unnamed_addr )?}}constant
// LIBRARY-DAG: @"$e7Library4SBoxVySiGWV" = {{(dso_local |dllexport |protected )?}}{{(local_unnamed_addr )?}}constant
// LIBRARY-DAG: @"$e7Library4EBoxOySiGMf" = {{(dso_local |dllexport |protected )?}}{{(local_unnamed_addr )?}}constant
// LIBRARY-DAG: @"$e7Library4EBoxOySiGWV" = {{(dso_local |dllexport |protected )?}}{{(local_unnamed_addr )?}}constant
// The vtable and deinit are the exported members.
// LIBRARY-DAG: @"$e7Library4CBoxCySiGMf" = {{(dso_local |dllexport |protected )?}}{{(local_unnamed_addr )?}}constant {{.*}} ptr @"$e7Library4CBoxCfDSi_Ts5", ptr null, ptr @"$e7Library4CBoxC1vxvgSi_Ts5", ptr @"$e7Library4CBoxC1vxvsSi_Ts5", ptr @"$e7Library4CBoxC1vxvMSi_Ts5", ptr @"$e7Library4CBoxCyACyxGxcfCSi_Ts5", ptr @"$e7Library4CBoxC3getxyFSi_Ts5" }>

// Protocol witness table; the generic requirement has no witness.
// LIBRARY-DAG: @"$e7Library4SBoxVySiGAA5ShapeAAWP" = {{(dso_local |dllexport |protected )?}}{{(local_unnamed_addr )?}}constant [3 x ptr] [ptr null, ptr @"{{[^"]+}}", ptr @swift_deletedMethodError]

// Members, including a satisfied constrained extension.
// LIBRARY-DAG: define {{(dso_local |dllexport |protected )?}}swiftcc {{.*}}@"$e7Library4SBoxVyACyxGxcfCSi_Ts5"
// LIBRARY-DAG: define {{(dso_local |dllexport |protected )?}}swiftcc {{.*}}@"$e7Library4SBoxVAASjRzlE7doubledxyFSi_Ts5"
// LIBRARY-DAG: define {{(dso_local |dllexport |protected )?}}swiftcc {{.*}}@"$e7Library4SBoxV4areaSiyFSi_Ts5"
// LIBRARY-DAG: define {{(dso_local |dllexport |protected )?}}swiftcc {{.*}}@"$e7Library4SBoxV5otherxyFSi_Ts5"
// LIBRARY-DAG: define {{(dso_local |dllexport |protected )?}}swiftcc {{.*}}@"$e7Library4SBoxV5otherxyFs5UInt8V_Ts5"

// No table for an unsatisfied conditional conformance or an internal protocol,
// and no members with their own generic parameters, from unsatisfied
// constrained extensions, or internal.
// LIBRARY-NOT-NOT: $e7Library4SBoxVySiG{{(AA4Text|AA6Hidden|SQ)}}AAWP
// LIBRARY-NOT-NOT: define {{.*}}$e7Library4SBoxV{{(3map|.*5upper|4aeic|8computed|4impl|11transparent|14transparentVar|6hidden|.*fA_|.*2eeoi)}}

// The vtable doesn't create its own copies of the exported members. (At -Onone,
// the serialized witness thunk of 'SBox<Int>: Shape' still creates its own copy
// of 'area', because inlinable code doesn't use exported specializations yet.)
// LIBRARY-O-NOT-NOT: define {{.*}}$e7Library4{{[SC]}}Box{{.*}}_Tg{{q?}}5"

//--- Application.swift

import Library

final class Sub: CBox<Int> { var extra = 1 }
public func make() -> [Any] { [SBox(1), EBox<Int>.a(2), CBox(3), Sub(4)] }
public func shape() -> any Shape { SBox(1) }
public func get(_ b: CBox<Int>) -> Int { b.get() + SBox(2).doubled() }

// APPLICATION-DAG: @"$e7Library4SBoxVySiGMf" = external global
// APPLICATION-DAG: @"$e7Library4EBoxOySiGMf" = external global
// APPLICATION-DAG: @"$e7Library4CBoxCySiGMf" = external global
// APPLICATION-DAG: @"$e7Library4SBoxVySiGAA5ShapeAAWP" = external global

// A subclass's superclass and inherited vtable entries are the library's.
// APPLICATION-DAG: @"$e11Application3SubCMf" = {{.*}} ptr @"$e7Library4CBoxCySiGMf"{{.*}} ptr @"$e7Library4CBoxC1vxvgSi_Ts5"{{.*}} ptr @"$e7Library4CBoxC3getxyFSi_Ts5" }>

// APPLICATION-NOT-NOT: define {{.*}}@"$e7Library
// APPLICATION-NOT-NOT: @"$e7Library{{[^"]*}}" = {{.*}}constant
