// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/Lib.swift \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -parse-as-library -emit-module -emit-module-path %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -O -wmo

// RUN: %target-sil-opt %t/Lib.swiftmodule -sil-verify-all -o %t/Lib-sil-opt.sil
// RUN: %FileCheck %s < %t/Lib-sil-opt.sil

// RUN: %target-build-swift -module-name=Main -package-name Pkg -I%t -emit-sil %t/main.swift -o %t/Main.sil
// RUN: %FileCheck %s --check-prefix=CHECK-MAIN < %t/Main.sil

// REQUIRES: swift_in_compiler

//--- main.swift

import Lib

// CHECK-MAIN-NOT: witness_method
// CHECK-MAIN-NOT: class_method
runPub([PubStruct(rawValue: 2), PubKlassZ(rawValue: 3)])
runPkg([PkgStruct(rawValue: 2), PkgKlassZ(rawValue: 3)])

 
//--- Lib.swift

public class ParentPubKlass {
  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib14ParentPubKlassC06parentC3VarSivg : $@convention(method) (@guaranteed ParentPubKlass) -> Int {
  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib14ParentPubKlassC06parentC3VarSivs : $@convention(method) (Int, @guaranteed ParentPubKlass) -> () {
  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib14ParentPubKlassC06parentC3VarSivM : $@yield_once @convention(method) (@guaranteed ParentPubKlass) -> @yields @inout Int {
  public var parentPubVar: Int

  public init(_ arg: Int) {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib14ParentPubKlassCyACSicfc : $@convention(method) (Int, @owned ParentPubKlass) -> @owned ParentPubKlass {
    parentPubVar = arg
  }

  public func parentPubFunc() {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib14ParentPubKlassC06parentC4FuncyyF : $@convention(method) (@guaranteed ParentPubKlass) -> () {
    print(parentPubVar)
  }
}

public class PubKlass: ParentPubKlass {
  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib8PubKlassC6pubVarSSvM : $@yield_once @convention(method) (@guaranteed PubKlass) -> @yields @inout String {
  public var pubVar: String = "publicVar"

  public init(_ arg: String) {
    // CHECK-DAG: sil [serialized] [exact_self_class] [canonical] @$s3Lib8PubKlassCyACSScfC : $@convention(method) (@owned String, @thick PubKlass.Type) -> @owned PubKlass {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib8PubKlassCyACSScfc : $@convention(method) (@owned String, @owned PubKlass) -> @owned PubKlass {
    super.init(1)
    pubVar = arg
  }

  public func pubFunc() {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib8PubKlassC7pubFuncyyF : $@convention(method) (@guaranteed PubKlass) -> () {
    print(pubVar)
  }

  override public func parentPubFunc() {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib8PubKlassC06parentB4FuncyyF : $@convention(method) (@guaranteed PubKlass) -> () {
    print(pubVar)
  }
}

public class ParentPubKlassWithInternalMemberX {
  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib33ParentPubKlassWithInternalMemberXC06parentC3VarSivM : $@yield_once @convention(method) (@guaranteed ParentPubKlassWithInternalMemberX) -> @yields @inout Int {
  public var parentPubVar: Int

  var parentIntVar: Int /// NOTE: internal, so not serialized

  public init(_ arg: Int) { /// NOTE: init definition doesn't have [serialized] as it contains an internal var

    // CHECK-DAG: sil [canonical] @$s3Lib33ParentPubKlassWithInternalMemberXCyACSicfc : $@convention(method) (Int, @owned ParentPubKlassWithInternalMemberX) -> @owned ParentPubKlassWithInternalMemberX {
    // CHECK-DAG: sil [serialized] [exact_self_class] [canonical] @$s3Lib33ParentPubKlassWithInternalMemberXCyACSicfC
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib33ParentPubKlassWithInternalMemberXCfd : $@convention(method) (@guaranteed ParentPubKlassWithInternalMemberX) -> @owned Builtin.NativeObject {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib33ParentPubKlassWithInternalMemberXCfD : $@convention(method) (@owned ParentPubKlassWithInternalMemberX) -> () {
    parentPubVar = arg
    parentIntVar = arg
  }
  public func parentPubFuncA() {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib33ParentPubKlassWithInternalMemberXC06parentC5FuncAyyF : $@convention(method) (@guaranteed ParentPubKlassWithInternalMemberX) -> () {
    print(parentPubVar)
  }

  public func parentPubFuncB() {
    // CHECK-DAG: sil [canonical] @$s3Lib33ParentPubKlassWithInternalMemberXC06parentC5FuncByyF : $@convention(method) (@guaranteed ParentPubKlassWithInternalMemberX) -> () {
    print(parentPubVar, parentIntVar) /// NOTE: definition can't be set [serialized] since it contains an internal var
  }

  func parentIntFunc() { /// NOTE: internal, so not serialized
    print(parentIntVar)
  }
}

public class PubKlassX: ParentPubKlassWithInternalMemberX {
  public var pubVar: String = "publicVar"

  override public var parentPubVar: Int {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib9PubKlassXC06parentB3VarSivg : $@convention(method) (@guaranteed PubKlassX) -> Int {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib9PubKlassXC06parentB3VarSivs : $@convention(method) (Int, @guaranteed PubKlassX) -> () {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib9PubKlassXC06parentB3VarSivM : $@yield_once @convention(method) (@guaranteed PubKlassX) -> @yields @inout Int {
    didSet { print ("newValue") }
  }

  public init() { /// NOTE: since it calls super.init which doesn't have [serialized], this init doesn't either.

    // CHECK-DAG: sil [canonical] @$s3Lib9PubKlassXCACycfc : $@convention(method) (@owned PubKlassX) -> @owned PubKlassX {
    // CHECK-DAG: sil [serialized] [exact_self_class] [canonical] @$s3Lib9PubKlassXCyACSicfC : $@convention(method) (Int, @thick PubKlassX.Type) -> @owned PubKlassX {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib9PubKlassXCfD : $@convention(method) (@owned PubKlassX) -> () {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib9PubKlassXCfd : $@convention(method) (@guaranteed PubKlassX) -> @owned Builtin.NativeObject {
    super.init(1)
  }

  override public func parentPubFuncA() {
    // CHECK-DAG: sil [canonical] @$s3Lib9PubKlassXC06parentB5FuncAyyF : $@convention(method) (@guaranteed PubKlassX) -> () {
    print(pubVar, parentIntVar)  /// NOTE: definition can't be set [serialized] since it contains an internal var
  }
  override public func parentPubFuncB() {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib9PubKlassXC06parentB5FuncByyF : $@convention(method) (@guaranteed PubKlassX) -> () {
    print(pubVar)
  }
  override func parentIntFunc() { /// NOTE: not serialized as it's internal
    print(pubVar)
  }
  public func pubFunc() {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib9PubKlassXC7pubFuncyyF : $@convention(method) (@guaranteed PubKlassX) -> () {
    print(pubVar)
  }
}

public class ParentPubKlassWithInternalMemberY {
  var parentIntVar: Int   /// NOTE: internal; not serialized

  public init(_ arg: Int) { /// NOTE: init definition does not have [serialized] as it contains an internal var

    // FIXME: init is not [serialized] but __allocating_init is; should both be not serialized?
    // CHECK-DAG: sil [canonical] @$s3Lib33ParentPubKlassWithInternalMemberYCyACSicfc : $@convention(method) (Int, @owned ParentPubKlassWithInternalMemberY) -> @owned ParentPubKlassWithInternalMemberY {
    // CHECK-DAG: sil [serialized] [exact_self_class] [canonical] @$s3Lib33ParentPubKlassWithInternalMemberYCyACSicfC : $@convention(method) (Int, @thick ParentPubKlassWithInternalMemberY.Type) -> @owned ParentPubKlassWithInternalMemberY {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib33ParentPubKlassWithInternalMemberYCfD : $@convention(method) (@owned ParentPubKlassWithInternalMemberY) -> () {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib33ParentPubKlassWithInternalMemberYCfd : $@convention(method) (@guaranteed ParentPubKlassWithInternalMemberY) -> @owned Builtin.NativeObject {
    parentIntVar = arg
  }

  func parentIntFunc() { /// NOTE: internal; not serialized
    print(parentIntVar)
  }
}

public class PubKlassY: ParentPubKlassWithInternalMemberY {
  public var pubVar: String = "publicVar"
  public init() { super.init(1) }
}

package class ParentPkgKlass {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib14ParentPkgKlassC06parentC3VarSivg : $@convention(method) (@guaranteed ParentPkgKlass) -> Int {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib14ParentPkgKlassC06parentC3VarSivs : $@convention(method) (Int, @guaranteed ParentPkgKlass) -> () {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib14ParentPkgKlassC06parentC3VarSivM : $@yield_once @convention(method) (@guaranteed ParentPkgKlass) -> @yields @inout Int {
  package var parentPkgVar: Int

  package init(_ arg: Int) {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib14ParentPkgKlassCyACSicfc : $@convention(method) (Int, @owned ParentPkgKlass) -> @owned ParentPkgKlass {
    parentPkgVar = arg
  }
  package func parentPkgFunc() -> Int {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib14ParentPkgKlassC06parentC4FuncSiyF : $@convention(method) (@guaranteed ParentPkgKlass) -> Int {
    parentPkgVar
  }
}

package class PkgKlass: ParentPkgKlass {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib8PkgKlassC6pkgVarSSvg : $@convention(method) (@guaranteed PkgKlass) -> @owned String {
  package var pkgVar: String = "pkgVar"

  package init(_ arg: String) {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassCyACSScfc : $@convention(method) (@owned String, @owned PkgKlass) -> @owned PkgKlass {
    super.init(1)
    pkgVar = arg
  }
  package func pkgFunc() { 
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassC7pkgFuncyyF : $@convention(method) (@guaranteed PkgKlass) -> () {
    print(pkgVar)
  }
  override package func parentPkgFunc() -> Int {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassC06parentB4FuncSiyF : $@convention(method) (@guaranteed PkgKlass) -> Int {
    pkgVar.count
  }
}

package class ParentPkgKlassWithInternalMemberX {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib33ParentPkgKlassWithInternalMemberXC06parentC3VarSivs : $@convention(method) (Int, @guaranteed ParentPkgKlassWithInternalMemberX) -> () {
  package var parentPkgVar: Int

  var parentIntVar: Int /// NOTE: internal so not serialized

  package init(_ arg: Int) {
    // FIXME: definition for .init is not generated at all; should it be generated (without [serialized]) like public?
    // CHECK-DAG: sil package_external [exact_self_class] [canonical] @$s3Lib33ParentPkgKlassWithInternalMemberXCyACSicfC : $@convention(method) (Int, @thick ParentPkgKlassWithInternalMemberX.Type) -> @owned ParentPkgKlassWithInternalMemberX {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib33ParentPkgKlassWithInternalMemberXCfD : $@convention(method) (@owned ParentPkgKlassWithInternalMemberX) -> () {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib33ParentPkgKlassWithInternalMemberXCfd : $@convention(method) (@guaranteed ParentPkgKlassWithInternalMemberX) -> @owned Builtin.NativeObject {
    parentPkgVar = arg
    parentIntVar = arg
  }

  package func parentPkgFuncA() {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib33ParentPkgKlassWithInternalMemberXC06parentC5FuncAyyF : $@convention(method) (@guaranteed ParentPkgKlassWithInternalMemberX) -> () {
    print(parentPkgVar)
  }
  package func parentPkgFuncB() { /// NOTE: not [serialized] as it contains an internal var
    // CHECK-DAG: sil package_external [canonical] @$s3Lib33ParentPkgKlassWithInternalMemberXC06parentC5FuncByyF : $@convention(method) (@guaranteed ParentPkgKlassWithInternalMemberX) -> () {
    print(parentPkgVar, parentIntVar)
  }
  func parentIntFunc() { /// NOTE: internal so not serialized
    print(parentIntVar)
  }
}

package class PkgKlassX: ParentPkgKlassWithInternalMemberX {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgKlassXC6pkgVarSSvs : $@convention(method) (@owned String, @guaranteed PkgKlassX) -> () {
  package var pkgVar: String = "pkgVar"

  override package var parentPkgVar: Int {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib9PkgKlassXC06parentB3VarSivg : $@convention(method) (@guaranteed PkgKlassX) -> Int {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib9PkgKlassXC06parentB3VarSivs : $@convention(method) (Int, @guaranteed PkgKlassX) -> () {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib9PkgKlassXC06parentB3VarSivM : $@yield_once @convention(method) (@guaranteed PkgKlassX) -> @yields @inout Int {
    didSet { print ("newValue") }
  }
  package init() {
    // FIXME: super.init is not [serialized], but this init definition should still be generated.

    /// NOTE: `__allocating_init` below is not [serialized].
    // CHECK-DAG: sil package_external [exact_self_class] [canonical] @$s3Lib9PkgKlassXCyACSicfC : $@convention(method) (Int, @thick PkgKlassX.Type) -> @owned PkgKlassX {
    super.init(1)
  }
  override package func parentPkgFuncA() {
    // CHECK-DAG: sil package_external [canonical] @$s3Lib9PkgKlassXC06parentB5FuncAyyF : $@convention(method) (@guaranteed PkgKlassX) -> () {
    print(pkgVar, parentIntVar)
  }
  override package func parentPkgFuncB() {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib9PkgKlassXC06parentB5FuncByyF : $@convention(method) (@guaranteed PkgKlassX) -> () {
    print(pkgVar)
  }
  override func parentIntFunc() { /// NOTE: not serialized
    print(pkgVar)
  }
  package func pubFunc() {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib9PkgKlassXC7pubFuncyyF : $@convention(method) (@guaranteed PkgKlassX) -> () {
    print(pkgVar)
  }
}

package class ParentPkgKlassWithInternalMemberY {
  var parentIntVar: Int
  package init(_ arg: Int) {
    // FIXME: only `__allocating_init` is generated.
    // CHECK-DAG: sil package_external [exact_self_class] [canonical] @$s3Lib33ParentPkgKlassWithInternalMemberYCyACSicfC : $@convention(method) (Int, @thick ParentPkgKlassWithInternalMemberY.Type) -> @owned ParentPkgKlassWithInternalMemberY {
    parentIntVar = arg
  }
  func parentIntFunc() {
    print(parentIntVar)
  }
}

package class PkgKlassY: ParentPkgKlassWithInternalMemberY {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgKlassYC6pkgVarSSvs : $@convention(method) (@owned String, @guaranteed PkgKlassY) -> () {
  package var pkgVar: String = "pkgVar"
  package init() {
    // FIXME: only `__allocating_init` is generated.
    // CHECK-DAG: sil package_external [exact_self_class] [canonical] @$s3Lib9PkgKlassYCACycfC : $@convention(method) (@thick PkgKlassY.Type) -> @owned PkgKlassY {
    super.init(1)
  }
}

public protocol PubProto {
  associatedtype Element = Self
  static var root: UInt16 { get }
  var env: UInt16 { get set }
  init(rawValue: UInt16)
  func pubFunc()
}

/// NOTE: witness thunks get `shared` linkage
public class PubKlassZ: PubProto {
  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PubKlassZCAA0B5ProtoA2aDP4roots6UInt16VvgZTW : $@convention(witness_method: PubProto) (@thick PubKlassZ.Type) -> UInt16 {
  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib9PubKlassZC4roots6UInt16VvgZ : $@convention(method) (@thick PubKlassZ.Type) -> UInt16 {
  public static let root: UInt16 = 1 << 0

  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PubKlassZCAA0B5ProtoA2aDP3envs6UInt16VvgTW : $@convention(witness_method: PubProto) (@in_guaranteed PubKlassZ) -> UInt16 {
  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PubKlassZCAA0B5ProtoA2aDP3envs6UInt16VvsTW : $@convention(witness_method: PubProto) (UInt16, @inout PubKlassZ) -> () {
  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PubKlassZCAA0B5ProtoA2aDP3envs6UInt16VvMTW : $@yield_once @convention(witness_method: PubProto) @substituted <τ_0_0> (@inout τ_0_0) -> @yields @inout UInt16 for <PubKlassZ> {
  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib9PubKlassZC3envs6UInt16Vvg : $@convention(method) (@guaranteed PubKlassZ) -> UInt16 {
  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib9PubKlassZC3envs6UInt16Vvs : $@convention(method) (UInt16, @guaranteed PubKlassZ) -> () {
  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib9PubKlassZC3envs6UInt16VvM : $@yield_once @convention(method) (@guaranteed PubKlassZ) -> @yields @inout UInt16 {
  public var env: UInt16

  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib9PubKlassZC8rawValues6UInt16Vvg : $@convention(method) (@guaranteed PubKlassZ) -> UInt16 {
  public let rawValue: UInt16

  required public init(rawValue: UInt16) {
    // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PubKlassZCAA0B5ProtoA2aDP8rawValuexs6UInt16V_tcfCTW : $@convention(witness_method: PubProto) (UInt16, @thick PubKlassZ.Type) -> @out PubKlassZ {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib9PubKlassZC8rawValueACs6UInt16V_tcfc : $@convention(method) (UInt16, @owned PubKlassZ) -> @owned PubKlassZ {
    self.rawValue = rawValue
    self.env = 1 << rawValue
  }
  public func pubFunc() {
    // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PubKlassZCAA0B5ProtoA2aDP7pubFuncyyFTW : $@convention(witness_method: PubProto) (@in_guaranteed PubKlassZ) -> () {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib9PubKlassZC7pubFuncyyF : $@convention(method) (@guaranteed PubKlassZ) -> () {
    print(env)
  }
}

public struct PubStruct: PubProto {
  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PubStructVAA0B5ProtoA2aDP4roots6UInt16VvgZTW : $@convention(witness_method: PubProto) (@thick PubStruct.Type) -> UInt16 {
  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib9PubStructV4roots6UInt16VvgZ : $@convention(method) (@thin PubStruct.Type) -> UInt16 {
  public static let root: UInt16 = 1 << 0

  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PubStructVAA0B5ProtoA2aDP3envs6UInt16VvgTW : $@convention(witness_method: PubProto) (@in_guaranteed PubStruct) -> UInt16 {
  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PubStructVAA0B5ProtoA2aDP3envs6UInt16VvsTW : $@convention(witness_method: PubProto) (UInt16, @inout PubStruct) -> () {
  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PubStructVAA0B5ProtoA2aDP3envs6UInt16VvMTW : $@yield_once @convention(witness_method: PubProto) @substituted <τ_0_0> (@inout τ_0_0) -> @yields @inout UInt16 for <PubStruct> {
  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib9PubStructV3envs6UInt16Vvg : $@convention(method) (PubStruct) -> UInt16 {
  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib9PubStructV3envs6UInt16Vvs : $@convention(method) (UInt16, @inout PubStruct) -> () {
  public var env: UInt16

  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib9PubStructV8rawValues6UInt16Vvg : $@convention(method) (PubStruct) -> UInt16 {
  public let rawValue: UInt16

  public init(rawValue: UInt16) {
    // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PubStructVAA0B5ProtoA2aDP8rawValuexs6UInt16V_tcfCTW : $@convention(witness_method: PubProto) (UInt16, @thick PubStruct.Type) -> @out PubStruct {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib9PubStructV8rawValueACs6UInt16V_tcfC : $@convention(method) (UInt16, @thin PubStruct.Type) -> PubStruct {
    self.rawValue = rawValue
    self.env = 1 << rawValue
  }

  public func pubFunc() {
    // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PubStructVAA0B5ProtoA2aDP7pubFuncyyFTW : $@convention(witness_method: PubProto) (@in_guaranteed PubStruct) -> () {
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib9PubStructV7pubFuncyyF : $@convention(method) (PubStruct) -> () {
    print(env)
  }
}

public protocol PubSimpleProto {
  var pubVar: Int { get set }
  func pubFunc() -> Int
}

struct InternalStruct {
  var name: String = "internalVar"
}

protocol InternalProto {
  var intVar: InternalStruct { get set }
  func intFunc() -> InternalStruct
}

public struct PubStructX: PubSimpleProto, InternalProto { /// NOTE: witness table serialized only for PubSimpleProto

  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib10PubStructXVAA0B11SimpleProtoA2aDP6pubVarSivgTW : $@convention(witness_method: PubSimpleProto) (@in_guaranteed PubStructX) -> Int {
  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib10PubStructXVAA0B11SimpleProtoA2aDP6pubVarSivsTW : $@convention(witness_method: PubSimpleProto) (Int, @inout PubStructX) -> () {
  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib10PubStructXVAA0B11SimpleProtoA2aDP6pubVarSivMTW : $@yield_once @convention(witness_method: PubSimpleProto) @substituted <τ_0_0> (@inout τ_0_0) -> @yields @inout Int for <PubStructX> {
  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib10PubStructXV6pubVarSivg : $@convention(method) (@guaranteed PubStructX) -> Int {
  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib10PubStructXV6pubVarSivs : $@convention(method) (Int, @inout PubStructX) -> () {
  // CHECK-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib10PubStructXV6pubVarSivM : $@yield_once @convention(method) (@inout PubStructX) -> @yields @inout Int {
  public var pubVar: Int

  var intVar: InternalStruct /// NOTE: internal; not serialized

  public init(_ arg: Int) { /// NOTE: init definition is not [serialized] as it contains an internal var
    // CHECK-DAG: sil [canonical] @$s3Lib10PubStructXVyACSicfC : $@convention(method) (Int, @thin PubStructX.Type) -> @owned PubStructX {
    self.intVar = InternalStruct(name: "foo")
    self.pubVar = arg
  }
  public func pubFunc() -> Int { 
    // CHECK-DAG: sil [serialized] [canonical] @$s3Lib10PubStructXV7pubFuncSiyF : $@convention(method) (@guaranteed PubStructX) -> Int {
    return pubVar
  }

  func intFunc() -> InternalStruct { /// NOTE: internal; not serialized
    return intVar
  }
}

public struct PubStructY: InternalProto {
  var intVar: InternalStruct
  public init() {
    // CHECK-DAG: sil [canonical] @$s3Lib10PubStructYVACycfC : $@convention(method) (@thin PubStructY.Type) -> @owned PubStructY {
    self.intVar = InternalStruct(name: "foo")
  }
  func intFunc() -> InternalStruct {
    return intVar
  }
}

package protocol PkgProto {
  associatedtype Element = Self
  static var root: UInt16 { get }
  var env: UInt16 { get set }
  init(rawValue: UInt16)
  func pkgFunc()
}

/// NOTE: witness thunks get `shared` linkage
package class PkgKlassZ: PkgProto {
  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PkgKlassZCAA0B5ProtoA2aDP4roots6UInt16VvgZTW : $@convention(witness_method: PkgProto) (@thick PkgKlassZ.Type) -> UInt16 {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgKlassZC4roots6UInt16VvgZ : $@convention(method) (@thick PkgKlassZ.Type) -> UInt16 {
  package static let root: UInt16 = 1 << 0

  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PkgKlassZCAA0B5ProtoA2aDP3envs6UInt16VvgTW : $@convention(witness_method: PkgProto) (@in_guaranteed PkgKlassZ) -> UInt16 {
  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PkgKlassZCAA0B5ProtoA2aDP3envs6UInt16VvsTW : $@convention(witness_method: PkgProto) (UInt16, @inout PkgKlassZ) -> () {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgKlassZC3envs6UInt16Vvg : $@convention(method) (@guaranteed PkgKlassZ) -> UInt16 {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgKlassZC3envs6UInt16Vvs : $@convention(method) (UInt16, @guaranteed PkgKlassZ) -> () {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgKlassZC3envs6UInt16VvM : $@yield_once @convention(method) (@guaranteed PkgKlassZ) -> @yields @inout UInt16 {
  package var env: UInt16

  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgKlassZC8rawValues6UInt16Vvg : $@convention(method) (@guaranteed PkgKlassZ) -> UInt16 {
  package let rawValue: UInt16

  required package init(rawValue: UInt16) {
    // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PkgKlassZCAA0B5ProtoA2aDP8rawValuexs6UInt16V_tcfCTW : $@convention(witness_method: PkgProto) (UInt16, @thick PkgKlassZ.Type) -> @out PkgKlassZ {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib9PkgKlassZC8rawValueACs6UInt16V_tcfc : $@convention(method) (UInt16, @owned PkgKlassZ) -> @owned PkgKlassZ {
    self.rawValue = rawValue
    self.env = 1 << rawValue
  }
  package func pkgFunc() {
    // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PkgKlassZCAA0B5ProtoA2aDP7pkgFuncyyFTW : $@convention(witness_method: PkgProto) (@in_guaranteed PkgKlassZ) -> () {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib9PkgKlassZC7pkgFuncyyF : $@convention(method) (@guaranteed PkgKlassZ) -> () {
    print(env)
  }
}

package struct PkgStruct: PkgProto { /// NOTE: witness thunks get `shared` linkage
  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PkgStructVAA0B5ProtoA2aDP4roots6UInt16VvgZTW : $@convention(witness_method: PkgProto) (@thick PkgStruct.Type) -> UInt16 {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgStructV4roots6UInt16VvgZ : $@convention(method) (@thin PkgStruct.Type) -> UInt16 {
  // FIXME: PkgStruct.root.unsafeMutableAddressor -- not generated for PubStruct; should this be removed?
  // CHECK-DAG: sil package_external [global_init] [canonical] @$s3Lib9PkgStructV4roots6UInt16Vvau : $@convention(thin) () -> Builtin.RawPointer {
  package static let root: UInt16 = 1 << 0

  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PkgStructVAA0B5ProtoA2aDP3envs6UInt16VvsTW : $@convention(witness_method: PkgProto) (UInt16, @inout PkgStruct) -> () {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgStructV3envs6UInt16Vvs : $@convention(method) (UInt16, @inout PkgStruct) -> () {
  package var env: UInt16

  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgStructV8rawValues6UInt16Vvg : $@convention(method) (PkgStruct) -> UInt16 {
  package let rawValue: UInt16

  package init(rawValue: UInt16) {
    // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib9PkgStructVAA0B5ProtoA2aDP8rawValuexs6UInt16V_tcfCTW : $@convention(witness_method: PkgProto) (UInt16, @thick PkgStruct.Type) -> @out PkgStruct {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib9PkgStructV8rawValueACs6UInt16V_tcfC : $@convention(method) (UInt16, @thin PkgStruct.Type) -> PkgStruct {
    self.rawValue = rawValue
    self.env = 1 << rawValue
  }
  package func pkgFunc() {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib9PkgStructV7pkgFuncyyF : $@convention(method) (PkgStruct) -> () {
    print(env)
  }
}

package protocol PkgSimpleProto {
  var pkgVar: Int { get set }
  func pkgFunc() -> Int
}

/// NOTE: only witness table of conformance to PkgSimpleProto is serialized.
package struct PkgStructX: PkgSimpleProto, InternalProto {
  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib10PkgStructXVAA0B11SimpleProtoA2aDP6pkgVarSivgTW : $@convention(witness_method: PkgSimpleProto) (@in_guaranteed PkgStructX) -> Int {
  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib10PkgStructXVAA0B11SimpleProtoA2aDP6pkgVarSivsTW : $@convention(witness_method: PkgSimpleProto) (Int, @inout PkgStructX) -> () {
  // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib10PkgStructXVAA0B11SimpleProtoA2aDP6pkgVarSivMTW : $@yield_once @convention(witness_method: PkgSimpleProto) @substituted <τ_0_0> (@inout τ_0_0) -> @yields @inout Int for <PkgStructX> {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib10PkgStructXV6pkgVarSivM : $@yield_once @convention(method) (@inout PkgStructX) -> @yields @inout Int {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib10PkgStructXV6pkgVarSivg : $@convention(method) (@guaranteed PkgStructX) -> Int {
  // CHECK-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib10PkgStructXV6pkgVarSivs : $@convention(method) (Int, @inout PkgStructX) -> () {
  package var pkgVar: Int
  var intVar: InternalStruct
  package init(_ arg: Int) {
    self.intVar = InternalStruct(name: "foo")
    self.pkgVar = arg
  }
  package func pkgFunc() -> Int { 
    // CHECK-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib10PkgStructXVAA0B11SimpleProtoA2aDP7pkgFuncSiyFTW : $@convention(witness_method: PkgSimpleProto) (@in_guaranteed PkgStructX) -> Int {
    // CHECK-DAG: sil package [serialized] [canonical] @$s3Lib10PkgStructXV7pkgFuncSiyF : $@convention(method) (@guaranteed PkgStructX) -> Int {
    return pkgVar
  }
  func intFunc() -> InternalStruct {
    return intVar
  }
}

/// NOTE: conforms to internal protocol, so not serialized
package struct PkgStructY: InternalProto {
  var intVar: InternalStruct
  package init() {
    // FIXME: definition for .init is not generated at all; should it be generated (without [serialized]) like public?
    self.intVar = InternalStruct(name: "foo")
  }
  func intFunc() -> InternalStruct {
    return intVar
  }
}

public func runPub(_ arg: [any PubProto]) {
  print(arg)
}

package func runPkg(_ arg: [any PkgProto]) {
  print(arg)
}

//CHECK-LABEL: sil_vtable [serialized] ParentPubKlass {
//CHECK-NEXT:  #ParentPubKlass.parentPubVar!getter: (ParentPubKlass) -> () -> Int : @$s3Lib14ParentPubKlassC06parentC3VarSivg
//CHECK-NEXT:  #ParentPubKlass.parentPubVar!setter: (ParentPubKlass) -> (Int) -> () : @$s3Lib14ParentPubKlassC06parentC3VarSivs
//CHECK-NEXT:  #ParentPubKlass.parentPubVar!modify: (ParentPubKlass) -> () -> () : @$s3Lib14ParentPubKlassC06parentC3VarSivM
//CHECK-NEXT:  #ParentPubKlass.init!allocator: (ParentPubKlass.Type) -> (Int) -> ParentPubKlass : @$s3Lib14ParentPubKlassCyACSicfC
//CHECK-NEXT:  #ParentPubKlass.parentPubFunc: (ParentPubKlass) -> () -> () : @$s3Lib14ParentPubKlassC06parentC4FuncyyF
//CHECK-NEXT:  #ParentPubKlass.deinit!deallocator: @$s3Lib14ParentPubKlassCfD

//CHECK-LABEL: sil_vtable [serialized] PubKlass {
//CHECK-NEXT:  #ParentPubKlass.parentPubVar!getter: (ParentPubKlass) -> () -> Int : @$s3Lib14ParentPubKlassC06parentC3VarSivg [inherited]
//CHECK-NEXT:  #ParentPubKlass.parentPubVar!setter: (ParentPubKlass) -> (Int) -> () : @$s3Lib14ParentPubKlassC06parentC3VarSivs [inherited]
//CHECK-NEXT:  #ParentPubKlass.parentPubVar!modify: (ParentPubKlass) -> () -> () : @$s3Lib14ParentPubKlassC06parentC3VarSivM [inherited]
//CHECK-NEXT:  #ParentPubKlass.init!allocator: (ParentPubKlass.Type) -> (Int) -> ParentPubKlass : @$s3Lib8PubKlassCyACSicfC [override]
//CHECK-NEXT:  #ParentPubKlass.parentPubFunc: (ParentPubKlass) -> () -> () : @$s3Lib8PubKlassC06parentB4FuncyyF [override]
//CHECK-NEXT:  #PubKlass.pubVar!getter: (PubKlass) -> () -> String : @$s3Lib8PubKlassC6pubVarSSvg
//CHECK-NEXT:  #PubKlass.pubVar!setter: (PubKlass) -> (String) -> () : @$s3Lib8PubKlassC6pubVarSSvs
//CHECK-NEXT:  #PubKlass.pubVar!modify: (PubKlass) -> () -> () : @$s3Lib8PubKlassC6pubVarSSvM
//CHECK-NEXT:  #PubKlass.init!allocator: (PubKlass.Type) -> (String) -> PubKlass : @$s3Lib8PubKlassCyACSScfC
//CHECK-NEXT:  #PubKlass.pubFunc: (PubKlass) -> () -> () : @$s3Lib8PubKlassC7pubFuncyyF
//CHECK-NEXT:  #PubKlass.deinit!deallocator: @$s3Lib8PubKlassCfD

//CHECK-LABEL: sil_vtable [serialized] ParentPubKlassWithInternalMemberX {
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberX.parentPubVar!getter: (ParentPubKlassWithInternalMemberX) -> () -> Int : @$s3Lib33ParentPubKlassWithInternalMemberXC06parentC3VarSivg
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberX.parentPubVar!setter: (ParentPubKlassWithInternalMemberX) -> (Int) -> () : @$s3Lib33ParentPubKlassWithInternalMemberXC06parentC3VarSivs
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberX.parentPubVar!modify: (ParentPubKlassWithInternalMemberX) -> () -> () : @$s3Lib33ParentPubKlassWithInternalMemberXC06parentC3VarSivM
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberX.init!allocator: (ParentPubKlassWithInternalMemberX.Type) -> (Int) -> ParentPubKlassWithInternalMemberX : @$s3Lib33ParentPubKlassWithInternalMemberXCyACSicfC
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberX.parentPubFuncA: (ParentPubKlassWithInternalMemberX) -> () -> () : @$s3Lib33ParentPubKlassWithInternalMemberXC06parentC5FuncAyyF
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberX.parentPubFuncB: (ParentPubKlassWithInternalMemberX) -> () -> () : @$s3Lib33ParentPubKlassWithInternalMemberXC06parentC5FuncByyF
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberX.deinit!deallocator: @$s3Lib33ParentPubKlassWithInternalMemberXCfD

//CHECK-LABEL: sil_vtable [serialized] PubKlassX {
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberX.parentPubVar!getter: (ParentPubKlassWithInternalMemberX) -> () -> Int : @$s3Lib9PubKlassXC06parentB3VarSivg [override]
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberX.parentPubVar!setter: (ParentPubKlassWithInternalMemberX) -> (Int) -> () : @$s3Lib9PubKlassXC06parentB3VarSivs [override]
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberX.parentPubVar!modify: (ParentPubKlassWithInternalMemberX) -> () -> () : @$s3Lib9PubKlassXC06parentB3VarSivM [override]
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberX.init!allocator: (ParentPubKlassWithInternalMemberX.Type) -> (Int) -> ParentPubKlassWithInternalMemberX : @$s3Lib9PubKlassXCyACSicfC [override]
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberX.parentPubFuncA: (ParentPubKlassWithInternalMemberX) -> () -> () : @$s3Lib9PubKlassXC06parentB5FuncAyyF [override]
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberX.parentPubFuncB: (ParentPubKlassWithInternalMemberX) -> () -> () : @$s3Lib9PubKlassXC06parentB5FuncByyF [override]
//CHECK-NEXT:  #PubKlassX.pubVar!getter: (PubKlassX) -> () -> String : @$s3Lib9PubKlassXC6pubVarSSvg
//CHECK-NEXT:  #PubKlassX.pubVar!setter: (PubKlassX) -> (String) -> () : @$s3Lib9PubKlassXC6pubVarSSvs
//CHECK-NEXT:  #PubKlassX.pubVar!modify: (PubKlassX) -> () -> () : @$s3Lib9PubKlassXC6pubVarSSvM
//CHECK-NEXT:  #PubKlassX.init!allocator: (PubKlassX.Type) -> () -> PubKlassX : @$s3Lib9PubKlassXCACycfC
//CHECK-NEXT:  #PubKlassX.pubFunc: (PubKlassX) -> () -> () : @$s3Lib9PubKlassXC7pubFuncyyF 
//CHECK-NEXT:  #PubKlassX.deinit!deallocator: @$s3Lib9PubKlassXCfD

//CHECK-LABEL: sil_vtable [serialized] ParentPubKlassWithInternalMemberY {
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberY.init!allocator: (ParentPubKlassWithInternalMemberY.Type) -> (Int) -> ParentPubKlassWithInternalMemberY : @$s3Lib33ParentPubKlassWithInternalMemberYCyACSicfC
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberY.deinit!deallocator: @$s3Lib33ParentPubKlassWithInternalMemberYCfD

//CHECK-LABEL: sil_vtable [serialized] PubKlassY {
//CHECK-NEXT:  #ParentPubKlassWithInternalMemberY.init!allocator: (ParentPubKlassWithInternalMemberY.Type) -> (Int) -> ParentPubKlassWithInternalMemberY : @$s3Lib9PubKlassYCyACSicfC [override]
//CHECK-NEXT:  #PubKlassY.pubVar!getter: (PubKlassY) -> () -> String : @$s3Lib9PubKlassYC6pubVarSSvg
//CHECK-NEXT:  #PubKlassY.pubVar!setter: (PubKlassY) -> (String) -> () : @$s3Lib9PubKlassYC6pubVarSSvs
//CHECK-NEXT:  #PubKlassY.pubVar!modify: (PubKlassY) -> () -> () : @$s3Lib9PubKlassYC6pubVarSSvM
//CHECK-NEXT:  #PubKlassY.init!allocator: (PubKlassY.Type) -> () -> PubKlassY : @$s3Lib9PubKlassYCACycfC
//CHECK-NEXT:  #PubKlassY.deinit!deallocator: @$s3Lib9PubKlassYCfD

//CHECK-LABEL: sil_vtable [serialized] ParentPkgKlass {
//CHECK-NEXT:  #ParentPkgKlass.parentPkgVar!getter: (ParentPkgKlass) -> () -> Int : @$s3Lib14ParentPkgKlassC06parentC3VarSivg  
//CHECK-NEXT:  #ParentPkgKlass.parentPkgVar!setter: (ParentPkgKlass) -> (Int) -> () : @$s3Lib14ParentPkgKlassC06parentC3VarSivs
//CHECK-NEXT:  #ParentPkgKlass.parentPkgVar!modify: (ParentPkgKlass) -> () -> () : @$s3Lib14ParentPkgKlassC06parentC3VarSivM
//CHECK-NEXT:  #ParentPkgKlass.init!allocator: (ParentPkgKlass.Type) -> (Int) -> ParentPkgKlass : @$s3Lib14ParentPkgKlassCyACSicfC
//CHECK-NEXT:  #ParentPkgKlass.parentPkgFunc: (ParentPkgKlass) -> () -> Int : @$s3Lib14ParentPkgKlassC06parentC4FuncSiyF
//CHECK-NEXT:  #ParentPkgKlass.deinit!deallocator: @$s3Lib14ParentPkgKlassCfD

//CHECK-LABEL: sil_vtable [serialized] PkgKlass {
//CHECK-NEXT:  #ParentPkgKlass.parentPkgVar!getter: (ParentPkgKlass) -> () -> Int : @$s3Lib14ParentPkgKlassC06parentC3VarSivg [inherited]
//CHECK-NEXT:  #ParentPkgKlass.parentPkgVar!setter: (ParentPkgKlass) -> (Int) -> () : @$s3Lib14ParentPkgKlassC06parentC3VarSivs [inherited]
//CHECK-NEXT:  #ParentPkgKlass.parentPkgVar!modify: (ParentPkgKlass) -> () -> () : @$s3Lib14ParentPkgKlassC06parentC3VarSivM [inherited]
//CHECK-NEXT:  #ParentPkgKlass.init!allocator: (ParentPkgKlass.Type) -> (Int) -> ParentPkgKlass : @$s3Lib8PkgKlassCyACSicfC [override]
//CHECK-NEXT:  #ParentPkgKlass.parentPkgFunc: (ParentPkgKlass) -> () -> Int : @$s3Lib8PkgKlassC06parentB4FuncSiyF [override]  
//CHECK-NEXT:  #PkgKlass.pkgVar!getter: (PkgKlass) -> () -> String : @$s3Lib8PkgKlassC6pkgVarSSvg
//CHECK-NEXT:  #PkgKlass.pkgVar!setter: (PkgKlass) -> (String) -> () : @$s3Lib8PkgKlassC6pkgVarSSvs
//CHECK-NEXT:  #PkgKlass.pkgVar!modify: (PkgKlass) -> () -> () : @$s3Lib8PkgKlassC6pkgVarSSvM
//CHECK-NEXT:  #PkgKlass.init!allocator: (PkgKlass.Type) -> (String) -> PkgKlass : @$s3Lib8PkgKlassCyACSScfC
//CHECK-NEXT:  #PkgKlass.pkgFunc: (PkgKlass) -> () -> () : @$s3Lib8PkgKlassC7pkgFuncyyF
//CHECK-NEXT:  #PkgKlass.deinit!deallocator: @$s3Lib8PkgKlassCfD

//CHECK-LABEL: sil_vtable [serialized] ParentPkgKlassWithInternalMemberX {
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberX.parentPkgVar!getter: (ParentPkgKlassWithInternalMemberX) -> () -> Int : @$s3Lib33ParentPkgKlassWithInternalMemberXC06parentC3VarSivg
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberX.parentPkgVar!setter: (ParentPkgKlassWithInternalMemberX) -> (Int) -> () : @$s3Lib33ParentPkgKlassWithInternalMemberXC06parentC3VarSivs
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberX.parentPkgVar!modify: (ParentPkgKlassWithInternalMemberX) -> () -> () : @$s3Lib33ParentPkgKlassWithInternalMemberXC06parentC3VarSivM
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberX.init!allocator: (ParentPkgKlassWithInternalMemberX.Type) -> (Int) -> ParentPkgKlassWithInternalMemberX : @$s3Lib33ParentPkgKlassWithInternalMemberXCyACSicfC
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberX.parentPkgFuncA: (ParentPkgKlassWithInternalMemberX) -> () -> () : @$s3Lib33ParentPkgKlassWithInternalMemberXC06parentC5FuncAyyF
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberX.parentPkgFuncB: (ParentPkgKlassWithInternalMemberX) -> () -> () : @$s3Lib33ParentPkgKlassWithInternalMemberXC06parentC5FuncByyF
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberX.deinit!deallocator: @$s3Lib33ParentPkgKlassWithInternalMemberXCfD

//CHECK-LABEL: sil_vtable [serialized] PkgKlassX {
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberX.parentPkgVar!getter: (ParentPkgKlassWithInternalMemberX) -> () -> Int : @$s3Lib9PkgKlassXC06parentB3VarSivg [override]
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberX.parentPkgVar!setter: (ParentPkgKlassWithInternalMemberX) -> (Int) -> () : @$s3Lib9PkgKlassXC06parentB3VarSivs [override]
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberX.parentPkgVar!modify: (ParentPkgKlassWithInternalMemberX) -> () -> () : @$s3Lib9PkgKlassXC06parentB3VarSivM [override]
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberX.init!allocator: (ParentPkgKlassWithInternalMemberX.Type) -> (Int) -> ParentPkgKlassWithInternalMemberX : @$s3Lib9PkgKlassXCyACSicfC [override]
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberX.parentPkgFuncA: (ParentPkgKlassWithInternalMemberX) -> () -> () : @$s3Lib9PkgKlassXC06parentB5FuncAyyF [override]
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberX.parentPkgFuncB: (ParentPkgKlassWithInternalMemberX) -> () -> () : @$s3Lib9PkgKlassXC06parentB5FuncByyF [override]
//CHECK-NEXT:  #PkgKlassX.pkgVar!getter: (PkgKlassX) -> () -> String : @$s3Lib9PkgKlassXC6pkgVarSSvg
//CHECK-NEXT:  #PkgKlassX.pkgVar!setter: (PkgKlassX) -> (String) -> () : @$s3Lib9PkgKlassXC6pkgVarSSvs
//CHECK-NEXT:  #PkgKlassX.pkgVar!modify: (PkgKlassX) -> () -> () : @$s3Lib9PkgKlassXC6pkgVarSSvM
//CHECK-NEXT:  #PkgKlassX.init!allocator: (PkgKlassX.Type) -> () -> PkgKlassX : @$s3Lib9PkgKlassXCACycfC
//CHECK-NEXT:  #PkgKlassX.pubFunc: (PkgKlassX) -> () -> () : @$s3Lib9PkgKlassXC7pubFuncyyF
//CHECK-NEXT:  #PkgKlassX.deinit!deallocator: @$s3Lib9PkgKlassXCfD

//CHECK-LABEL: sil_vtable [serialized] ParentPkgKlassWithInternalMemberY {
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberY.init!allocator: (ParentPkgKlassWithInternalMemberY.Type) -> (Int) -> ParentPkgKlassWithInternalMemberY : @$s3Lib33ParentPkgKlassWithInternalMemberYCyACSicfC
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberY.deinit!deallocator: @$s3Lib33ParentPkgKlassWithInternalMemberYCfD

//CHECK-LABEL: sil_vtable [serialized] PkgKlassY {
//CHECK-NEXT:  #ParentPkgKlassWithInternalMemberY.init!allocator: (ParentPkgKlassWithInternalMemberY.Type) -> (Int) -> ParentPkgKlassWithInternalMemberY : @$s3Lib9PkgKlassYCyACSicfC [override]
//CHECK-NEXT:  #PkgKlassY.pkgVar!getter: (PkgKlassY) -> () -> String : @$s3Lib9PkgKlassYC6pkgVarSSvg
//CHECK-NEXT:  #PkgKlassY.pkgVar!setter: (PkgKlassY) -> (String) -> () : @$s3Lib9PkgKlassYC6pkgVarSSvs
//CHECK-NEXT:  #PkgKlassY.pkgVar!modify: (PkgKlassY) -> () -> () : @$s3Lib9PkgKlassYC6pkgVarSSvM
//CHECK-NEXT:  #PkgKlassY.init!allocator: (PkgKlassY.Type) -> () -> PkgKlassY : @$s3Lib9PkgKlassYCACycfC
//CHECK-NEXT:  #PkgKlassY.deinit!deallocator: @$s3Lib9PkgKlassYCfD

//CHECK-LABEL: sil_vtable [serialized] PubKlassZ {
//CHECK-NEXT:  #PubKlassZ.env!getter: (PubKlassZ) -> () -> UInt16 : @$s3Lib9PubKlassZC3envs6UInt16Vvg
//CHECK-NEXT:  #PubKlassZ.env!setter: (PubKlassZ) -> (UInt16) -> () : @$s3Lib9PubKlassZC3envs6UInt16Vvs
//CHECK-NEXT:  #PubKlassZ.env!modify: (PubKlassZ) -> () -> () : @$s3Lib9PubKlassZC3envs6UInt16VvM
//CHECK-NEXT:  #PubKlassZ.init!allocator: (PubKlassZ.Type) -> (UInt16) -> PubKlassZ : @$s3Lib9PubKlassZC8rawValueACs6UInt16V_tcfC
//CHECK-NEXT:  #PubKlassZ.pubFunc: (PubKlassZ) -> () -> () : @$s3Lib9PubKlassZC7pubFuncyyF
//CHECK-NEXT:  #PubKlassZ.deinit!deallocator: @$s3Lib9PubKlassZCfD

//CHECK-LABEL: sil_vtable [serialized] PkgKlassZ {
//CHECK-NEXT:  #PkgKlassZ.env!getter: (PkgKlassZ) -> () -> UInt16 : @$s3Lib9PkgKlassZC3envs6UInt16Vvg
//CHECK-NEXT:  #PkgKlassZ.env!setter: (PkgKlassZ) -> (UInt16) -> () : @$s3Lib9PkgKlassZC3envs6UInt16Vvs
//CHECK-NEXT:  #PkgKlassZ.env!modify: (PkgKlassZ) -> () -> () : @$s3Lib9PkgKlassZC3envs6UInt16VvM
//CHECK-NEXT:  #PkgKlassZ.init!allocator: (PkgKlassZ.Type) -> (UInt16) -> PkgKlassZ : @$s3Lib9PkgKlassZC8rawValueACs6UInt16V_tcfC 
//CHECK-NEXT:  #PkgKlassZ.pkgFunc: (PkgKlassZ) -> () -> () : @$s3Lib9PkgKlassZC7pkgFuncyyF
//CHECK-NEXT:  #PkgKlassZ.deinit!deallocator: @$s3Lib9PkgKlassZCfD

//CHECK-LABEL:  sil_witness_table [serialized] PubKlassZ: PubProto module Lib {
//CHECK-NEXT:   associated_type Element: PubKlassZ
//CHECK-NEXT:   method #PubProto.root!getter: <Self where Self : PubProto> (Self.Type) -> () -> UInt16 : @$s3Lib9PubKlassZCAA0B5ProtoA2aDP4roots6UInt16VvgZTW
//CHECK-NEXT:   method #PubProto.env!getter: <Self where Self : PubProto> (Self) -> () -> UInt16 : @$s3Lib9PubKlassZCAA0B5ProtoA2aDP3envs6UInt16VvgTW
//CHECK-NEXT:   method #PubProto.env!setter: <Self where Self : PubProto> (inout Self) -> (UInt16) -> () : @$s3Lib9PubKlassZCAA0B5ProtoA2aDP3envs6UInt16VvsTW
//CHECK-NEXT:   method #PubProto.env!modify: <Self where Self : PubProto> (inout Self) -> () -> () : @$s3Lib9PubKlassZCAA0B5ProtoA2aDP3envs6UInt16VvMTW
//CHECK-NEXT:   method #PubProto.init!allocator: <Self where Self : PubProto> (Self.Type) -> (UInt16) -> Self : @$s3Lib9PubKlassZCAA0B5ProtoA2aDP8rawValuexs6UInt16V_tcfCTW
//CHECK-NEXT:   method #PubProto.pubFunc: <Self where Self : PubProto> (Self) -> () -> () : @$s3Lib9PubKlassZCAA0B5ProtoA2aDP7pubFuncyyFTW

//CHECK-LABEL:  sil_witness_table [serialized] PubStruct: PubProto module Lib {
//CHECK-NEXT:   associated_type Element: PubStruct
//CHECK-NEXT:   method #PubProto.root!getter: <Self where Self : PubProto> (Self.Type) -> () -> UInt16 : @$s3Lib9PubStructVAA0B5ProtoA2aDP4roots6UInt16VvgZTW
//CHECK-NEXT:   method #PubProto.env!getter: <Self where Self : PubProto> (Self) -> () -> UInt16 : @$s3Lib9PubStructVAA0B5ProtoA2aDP3envs6UInt16VvgTW
//CHECK-NEXT:   method #PubProto.env!setter: <Self where Self : PubProto> (inout Self) -> (UInt16) -> () : @$s3Lib9PubStructVAA0B5ProtoA2aDP3envs6UInt16VvsTW
//CHECK-NEXT:   method #PubProto.env!modify: <Self where Self : PubProto> (inout Self) -> () -> () : @$s3Lib9PubStructVAA0B5ProtoA2aDP3envs6UInt16VvMTW
//CHECK-NEXT:   method #PubProto.init!allocator: <Self where Self : PubProto> (Self.Type) -> (UInt16) -> Self : @$s3Lib9PubStructVAA0B5ProtoA2aDP8rawValuexs6UInt16V_tcfCTW
//CHECK-NEXT:   method #PubProto.pubFunc: <Self where Self : PubProto> (Self) -> () -> () : @$s3Lib9PubStructVAA0B5ProtoA2aDP7pubFuncyyFTW

//CHECK-LABEL:  sil_witness_table [serialized] PubStructX: PubSimpleProto module Lib {
//CHECK-NEXT:   method #PubSimpleProto.pubVar!getter: <Self where Self : PubSimpleProto> (Self) -> () -> Int : @$s3Lib10PubStructXVAA0B11SimpleProtoA2aDP6pubVarSivgTW
//CHECK-NEXT:   method #PubSimpleProto.pubVar!setter: <Self where Self : PubSimpleProto> (inout Self) -> (Int) -> () : @$s3Lib10PubStructXVAA0B11SimpleProtoA2aDP6pubVarSivsTW
//CHECK-NEXT:   method #PubSimpleProto.pubVar!modify: <Self where Self : PubSimpleProto> (inout Self) -> () -> () : @$s3Lib10PubStructXVAA0B11SimpleProtoA2aDP6pubVarSivMTW
//CHECK-NEXT:   method #PubSimpleProto.pubFunc: <Self where Self : PubSimpleProto> (Self) -> () -> Int : @$s3Lib10PubStructXVAA0B11SimpleProtoA2aDP7pubFuncSiyFTW

//CHECK-LABEL:  sil_witness_table package [serialized] PkgKlassZ: PkgProto module Lib {
//CHECK-NEXT:   associated_type Element: PkgKlassZ
//CHECK-NEXT:   method #PkgProto.root!getter: <Self where Self : PkgProto> (Self.Type) -> () -> UInt16 : @$s3Lib9PkgKlassZCAA0B5ProtoA2aDP4roots6UInt16VvgZTW
//CHECK-NEXT:   method #PkgProto.env!getter: <Self where Self : PkgProto> (Self) -> () -> UInt16 : @$s3Lib9PkgKlassZCAA0B5ProtoA2aDP3envs6UInt16VvgTW
//CHECK-NEXT:   method #PkgProto.env!setter: <Self where Self : PkgProto> (inout Self) -> (UInt16) -> () : @$s3Lib9PkgKlassZCAA0B5ProtoA2aDP3envs6UInt16VvsTW
//CHECK-NEXT:   method #PkgProto.env!modify: <Self where Self : PkgProto> (inout Self) -> () -> () : @$s3Lib9PkgKlassZCAA0B5ProtoA2aDP3envs6UInt16VvMTW
//CHECK-NEXT:   method #PkgProto.init!allocator: <Self where Self : PkgProto> (Self.Type) -> (UInt16) -> Self : @$s3Lib9PkgKlassZCAA0B5ProtoA2aDP8rawValuexs6UInt16V_tcfCTW
//CHECK-NEXT:   method #PkgProto.pkgFunc: <Self where Self : PkgProto> (Self) -> () -> () : @$s3Lib9PkgKlassZCAA0B5ProtoA2aDP7pkgFuncyyFTW

//CHECK-LABEL:  sil_witness_table package [serialized] PkgStruct: PkgProto module Lib {
//CHECK-NEXT:   associated_type Element: PkgStruct
//CHECK-NEXT:  method #PkgProto.root!getter: <Self where Self : PkgProto> (Self.Type) -> () -> UInt16 : @$s3Lib9PkgStructVAA0B5ProtoA2aDP4roots6UInt16VvgZTW
//CHECK-NEXT:   method #PkgProto.env!getter: <Self where Self : PkgProto> (Self) -> () -> UInt16 : @$s3Lib9PkgStructVAA0B5ProtoA2aDP3envs6UInt16VvgTW
//CHECK-NEXT:   method #PkgProto.env!setter: <Self where Self : PkgProto> (inout Self) -> (UInt16) -> () : @$s3Lib9PkgStructVAA0B5ProtoA2aDP3envs6UInt16VvsTW
//CHECK-NEXT:   method #PkgProto.env!modify: <Self where Self : PkgProto> (inout Self) -> () -> () : @$s3Lib9PkgStructVAA0B5ProtoA2aDP3envs6UInt16VvMTW
//CHECK-NEXT:   method #PkgProto.init!allocator: <Self where Self : PkgProto> (Self.Type) -> (UInt16) -> Self : @$s3Lib9PkgStructVAA0B5ProtoA2aDP8rawValuexs6UInt16V_tcfCTW
//CHECK-NEXT:   method #PkgProto.pkgFunc: <Self where Self : PkgProto> (Self) -> () -> () : @$s3Lib9PkgStructVAA0B5ProtoA2aDP7pkgFuncyyFTW

//CHECK-LABEL:  sil_witness_table package [serialized] PkgStructX: PkgSimpleProto module Lib {
//CHECK-NEXT:   method #PkgSimpleProto.pkgVar!getter: <Self where Self : PkgSimpleProto> (Self) -> () -> Int : @$s3Lib10PkgStructXVAA0B11SimpleProtoA2aDP6pkgVarSivgTW 
//CHECK-NEXT:   method #PkgSimpleProto.pkgVar!setter: <Self where Self : PkgSimpleProto> (inout Self) -> (Int) -> () : @$s3Lib10PkgStructXVAA0B11SimpleProtoA2aDP6pkgVarSivsTW
//CHECK-NEXT:   method #PkgSimpleProto.pkgVar!modify: <Self where Self : PkgSimpleProto> (inout Self) -> () -> () : @$s3Lib10PkgStructXVAA0B11SimpleProtoA2aDP6pkgVarSivMTW
//CHECK-NEXT:   method #PkgSimpleProto.pkgFunc: <Self where Self : PkgSimpleProto> (Self) -> () -> Int : @$s3Lib10PkgStructXVAA0B11SimpleProtoA2aDP7pkgFuncSiyFTW
