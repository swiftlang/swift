// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build Utils module resiliently.
// RUN: %target-swift-frontend -emit-module %t/Utils.swift \
// RUN:   -module-name Utils -swift-version 5 -I %t \
// RUN:   -package-name mypkg \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module -emit-module-path %t/Utils.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t -swift-version 5 -package-name mypkg -verify

/// Check serialization in SILGEN with resilience enabled.
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -emit-verbose-sil -sil-verify-all -enable-library-evolution -module-name Utils %t/Utils.swift -package-name mypkg -I %t > %t/Utils-Res.sil
// RUN: %FileCheck %s --check-prefixes=UTILS-RES,UTILS-COMMON -input-file=%t/Utils-Res.sil

/// Check for indirect access with a resiliently built module dependency.
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -sil-verify-all %t/Client.swift -package-name mypkg -I %t > %t/Client-Res.sil
// RUN: %FileCheck %s --check-prefixes=CLIENT-RES,CLIENT-COMMON -input-file=%t/Client-Res.sil

// RUN: rm -rf %t/Utils.swiftmodule

/// Build Utils module non-resiliently
// RUN: %target-swift-frontend -emit-module %t/Utils.swift \
// RUN:   -module-name Utils -swift-version 5 -I %t \
// RUN:   -package-name mypkg \
// RUN:   -emit-module -emit-module-path %t/Utils.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t -swift-version 5 -package-name mypkg -verify

/// Check serialization in SILGEN with resilience not enabled.
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -emit-verbose-sil -sil-verify-all -module-name Utils %t/Utils.swift -package-name mypkg -I %t > %t/Utils-NonRes.sil
// RUN: %FileCheck %s --check-prefixes=UTILS-NONRES,UTILS-COMMON -input-file %t/Utils-NonRes.sil

/// Check for indirect access with a non-resiliently built module dependency.
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -sil-verify-all %t/Client.swift -package-name mypkg -I %t > %t/Client-NonRes.sil
// RUN: %FileCheck %s --check-prefixes=CLIENT-NONRES,CLIENT-COMMON -input-file %t/Client-NonRes.sil


//--- Utils.swift

// UTILS-NONRES-NOT: sil shared [thunk] [ossa] @$s5Utils11PublicKlassC4dataSivpACTk : $@convention(keypath_accessor_setter) (@in_guaranteed Int, @in_guaranteed PublicKlass) -> () {
// UTILS-NONRES-NOT: sil shared [thunk] [ossa] @$s5Utils11PublicKlassC4dataSivpACTk : $@convention(keypath_accessor_setter) (@in_guaranteed Int, @in_guaranteed PublicKlass) -> () {

// UTILS-NONRES-NOT: sil shared [thunk] [ossa] @$s5Utils8PkgKlassC4dataSivpACTK : $@convention(keypath_accessor_getter) (@in_guaranteed PkgKlass) -> @out Int {
// UTILS-NONRES-NOT: sil shared [thunk] [ossa] @$s5Utils8PkgKlassC4dataSivpACTk : $@convention(keypath_accessor_setter) (@in_guaranteed Int, @in_guaranteed PkgKlass) -> () {

// UTILS-NONRES-NOT: sil shared [thunk] [ossa] @$s5Utils11UfiPkgClassC4dataSivpACTK : $@convention(keypath_accessor_getter) (@in_guaranteed UfiPkgClass) -> @out Int {
// UTILS-NONRES-NOT: sil shared [thunk] [ossa] @$s5Utils11UfiPkgClassC4dataSivpACTk : $@convention(keypath_accessor_setter) (@in_guaranteed Int, @in_guaranteed UfiPkgClass) -> () {

// UTILS-NONRES-NOT: sil shared [thunk] [ossa] @$s5Utils15PkgClassGenericC4dataxvplACyxGTK : $@convention(keypath_accessor_getter) <T> (@in_guaranteed PkgClassGeneric<T>) -> @out T {
// UTILS-NONRES-NOT: sil shared [thunk] [ossa] @$s5Utils15PkgClassGenericC4dataxvplACyxGTk : $@convention(keypath_accessor_setter) <T> (@in_guaranteed T, @in_guaranteed PkgClassGeneric<T>) -> () {

public protocol PublicProto {
  var data: Int { get set }
  func pfunc(_ arg: Int) -> Int
}

public class PublicKlass: PublicProto {
  // key path getter/setter for PublicKlass are only emitted when resilient.
  // key path getter for PublicKlass.data : PublicKlass
  // UTILS-RES-DAG: sil shared [thunk] [ossa] @$s5Utils11PublicKlassC4dataSivpACTK : $@convention(keypath_accessor_getter) (@in_guaranteed PublicKlass) -> @out Int {

  // key path setter for PublicKlass.data : PublicKlass
  // UTILS-RES-DAG: sil shared [thunk] [ossa] @$s5Utils11PublicKlassC4dataSivpACTk : $@convention(keypath_accessor_setter) (@in_guaranteed Int, @in_guaranteed PublicKlass) -> () {

  // PublicKlass.data.getter
  // UTILS-RES-DAG: sil [ossa] @$s5Utils11PublicKlassC4dataSivg : $@convention(method) (@guaranteed PublicKlass) -> Int
  // UTILS-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s5Utils11PublicKlassC4dataSivg : $@convention(method) (@guaranteed PublicKlass) -> Int {

  // PublicKlass.data.setter
  // UTILS-RES-DAG: sil [ossa] @$s5Utils11PublicKlassC4dataSivs : $@convention(method) (Int, @guaranteed PublicKlass) -> () {
  // UTILS-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s5Utils11PublicKlassC4dataSivs : $@convention(method) (Int, @guaranteed PublicKlass) -> () {

  // PublicKlass.data.modify
  // UTILS-RES-DAG: sil [ossa] @$s5Utils11PublicKlassC4dataSivM : $@yield_once @convention(method) (@guaranteed PublicKlass) -> @yields @inout Int {
  // UTILS-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s5Utils11PublicKlassC4dataSivM : $@yield_once @convention(method) (@guaranteed PublicKlass) -> @yields @inout Int {

  // protocol witness for PublicProto.data.getter in conformance PublicKlass
  // UTILS-RES-DAG: sil private [transparent] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP4dataSivgTW : $@convention(witness_method: PublicProto) (@in_guaranteed PublicKlass) -> Int {
  // UTILS-NONRES-DAG: sil shared [transparent] [serialized] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP4dataSivgTW : $@convention(witness_method: PublicProto) (@in_guaranteed PublicKlass) -> Int {

  // protocol witness for PublicProto.data.setter in conformance PublicKlass
  // UTILS-RES-DAG: sil private [transparent] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP4dataSivsTW : $@convention(witness_method: PublicProto) (Int, @inout PublicKlass) -> () {
  // UTILS-NONRES-DAG: sil shared [transparent] [serialized] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP4dataSivsTW : $@convention(witness_method: PublicProto) (Int, @inout PublicKlass) -> () {

  // protocol witness for PublicProto.data.modify in conformance PublicKlass
  // UTILS-RES-DAG: sil private [transparent] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP4dataSivMTW : $@yield_once @convention(witness_method: PublicProto) @substituted <τ_0_0> (@inout τ_0_0) -> @yields @inout Int for <PublicKlass> {
  // UTILS-NONRES-DAG: sil shared [transparent] [serialized] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP4dataSivMTW : $@yield_once @convention(witness_method: PublicProto) @substituted <τ_0_0> (@inout τ_0_0) -> @yields @inout Int for <PublicKlass> {
  public var data: Int

  // default argument 0 of PublicKlass.init(data:)
  // UTILS-COMMON-DAG: sil non_abi [serialized] [ossa] @$s5Utils11PublicKlassC4dataACSi_tcfcfA_ : $@convention(thin) () -> Int {

  // PublicKlass.__allocating_init(data:)
  // UTILS-COMMON-DAG: sil [serialized] [exact_self_class] [ossa] @$s5Utils11PublicKlassC4dataACSi_tcfC : $@convention(method) (Int, @thick PublicKlass.Type) -> @owned PublicKlass {
  public init(data: Int = 1) {
    self.data = data
  }

  // protocol witness for PublicProto.pfunc(_:) in conformance PublicKlass
  // UTILS-RES-DAG: sil private [transparent] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP5pfuncyS2iFTW : $@convention(witness_method: PublicProto) (Int, @in_guaranteed PublicKlass) -> Int {
  // UTILS-NONRES-DAG: sil shared [transparent] [serialized] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP5pfuncyS2iFTW : $@convention(witness_method: PublicProto) (Int, @in_guaranteed PublicKlass) -> Int {
  public func pfunc(_ arg: Int) -> Int {
    return data + arg
  }
}

package protocol PkgProto {
  var data: Int { get set }
  func pkgfunc(_ arg: Int) -> Int
}

package class PkgKlass: PkgProto {
  // Similar to PublicKlass, key path getter/setter for PkgKlass are only emitted 
  // when resilient.

  // key path getter for PkgKlass.data : PkgKlass
  // UTILS-RES-DAG: sil shared [thunk] [ossa] @$s5Utils8PkgKlassC4dataSivpACTK : $@convention(keypath_accessor_getter) (@in_guaranteed PkgKlass) -> @out Int {

  // key path setter for PkgKlass.data : PkgKlass
  // UTILS-RES-DAG: sil shared [thunk] [ossa] @$s5Utils8PkgKlassC4dataSivpACTk : $@convention(keypath_accessor_setter) (@in_guaranteed Int, @in_guaranteed PkgKlass) -> () {

  // protocol witness for PkgProto.data.getter in conformance PkgKlass
  // UTILS-COMMON-DAG: sil private [transparent] [thunk] [ossa] @$s5Utils8PkgKlassCAA0B5ProtoA2aDP4dataSivgTW : $@convention(witness_method: PkgProto) (@in_guaranteed PkgKlass) -> Int {

  // protocol witness for PkgProto.data.setter in conformance PkgKlass
  // UTILS-COMMON-DAG: sil private [transparent] [thunk] [ossa] @$s5Utils8PkgKlassCAA0B5ProtoA2aDP4dataSivsTW : $@convention(witness_method: PkgProto) (Int, @inout PkgKlass) -> () {

  // PkgKlass.data.getter
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils8PkgKlassC4dataSivg : $@convention(method) (@guaranteed PkgKlass) -> Int {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils8PkgKlassC4dataSivg : $@convention(method) (@guaranteed PkgKlass) -> Int {

  // PkgKlass.data.setter
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils8PkgKlassC4dataSivs : $@convention(method) (Int, @guaranteed PkgKlass) -> () {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils8PkgKlassC4dataSivs : $@convention(method) (Int, @guaranteed PkgKlass) -> () {

  // PkgKlass.data.modify
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils8PkgKlassC4dataSivM : $@yield_once @convention(method) (@guaranteed PkgKlass) -> @yields @inout Int {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils8PkgKlassC4dataSivM : $@yield_once @convention(method) (@guaranteed PkgKlass) -> @yields @inout Int {

  // protocol witness for PkgProto.pkgfunc(_:) in conformance PkgKlass
  // UTILS-COMMON-DAG: sil private [transparent] [thunk] [ossa] @$s5Utils8PkgKlassCAA0B5ProtoA2aDP7pkgfuncyS2iFTW : $@convention(witness_method: PkgProto) (Int, @in_guaranteed PkgKlass) -> Int {
  package var data: Int

  // default argument 0 of PkgKlass.init(data:)
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils8PkgKlassC4dataACSi_tcfcfA_ : $@convention(thin) () -> Int {

  // PkgKlass.__allocating_init(data:)
  // UTILS-COMMON-DAG: sil package [exact_self_class] [ossa] @$s5Utils8PkgKlassC4dataACSi_tcfC : $@convention(method) (Int, @thick PkgKlass.Type) -> @owned PkgKlass {

  // PkgKlass.init(data:)
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils8PkgKlassC4dataACSi_tcfc : $@convention(method) (Int, @owned PkgKlass) -> @owned PkgKlass {

  // PkgKlass.deinit
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils8PkgKlassCfd : $@convention(method) (@guaranteed PkgKlass) -> @owned Builtin.NativeObject {

  // PkgKlass.__deallocating_deinit
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils8PkgKlassCfD : $@convention(method) (@owned PkgKlass) -> () {
  package init(data: Int = 1) {
    self.data = data
  }

  // PkgKlass.pkgfunc(_:)
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils8PkgKlassC7pkgfuncyS2iF : $@convention(method) (Int, @guaranteed PkgKlass) -> Int {
  package func pkgfunc(_ arg: Int) -> Int {
    return data + arg
  }
}

class InternalClass {
  // InternalClass.data.getter
  // UTILS-COMMON-DAG: sil hidden [transparent] [ossa] @$s5Utils13InternalClassC4dataSivg : $@convention(method) (@guaranteed InternalClass) -> Int {

  // InternalClass.data.setter
  // UTILS-COMMON-DAG: sil hidden [transparent] [ossa] @$s5Utils13InternalClassC4dataSivs : $@convention(method) (Int, @guaranteed InternalClass) -> () {

  // InternalClass.data.modify
  // UTILS-COMMON-DAG: sil hidden [transparent] [ossa] @$s5Utils13InternalClassC4dataSivM : $@yield_once @convention(method) (@guaranteed InternalClass) -> @yields @inout Int {
  var data: Int

  // default argument 0 of InternalClass.init(data:)
  // UTILS-COMMON-DAG: sil hidden [ossa] @$s5Utils13InternalClassC4dataACSi_tcfcfA_ : $@convention(thin) () -> Int {

  // InternalClass.__allocating_init(data:)
  // UTILS-COMMON-DAG: sil hidden [exact_self_class] [ossa] @$s5Utils13InternalClassC4dataACSi_tcfC : $@convention(method) (Int, @thick InternalClass.Type) -> @owned InternalClass {

  // InternalClass.init(data:)
  // UTILS-COMMON-DAG: sil hidden [ossa] @$s5Utils13InternalClassC4dataACSi_tcfc : $@convention(method) (Int, @owned InternalClass) -> @owned InternalClass {

  // InternalClass.deinit
  // UTILS-COMMON-DAG: sil hidden [ossa] @$s5Utils13InternalClassCfd : $@convention(method) (@guaranteed InternalClass) -> @owned Builtin.NativeObject {

  // InternalClass.__deallocating_deinit
  // UTILS-COMMON-DAG: sil hidden [ossa] @$s5Utils13InternalClassCfD : $@convention(method) (@owned InternalClass) -> () {
  init(data: Int = 1) {
    self.data = data
  }

  // InternalClass.internalfunc(_:)
  // UTILS-COMMON-DAG: sil hidden [ossa] @$s5Utils13InternalClassC12internalfuncyS2iF : $@convention(method) (Int, @guaranteed InternalClass) -> Int {
  func internalfunc(_ arg: Int) -> Int {
    return data + arg
  }
}

@usableFromInline
package class UfiPkgClass {
  // variable initialization expression of UfiPkgClass.data
  // UTILS-RES-DAG: sil hidden [transparent] [ossa] @$s5Utils11UfiPkgClassC4dataSivpfi : $@convention(thin) () -> Int {
  // UTILS-NONRES-DAG: sil [transparent] [ossa] @$s5Utils11UfiPkgClassC4dataSivpfi : $@convention(thin) () -> Int {

  // Similar to PublicKlass and PkgKlass, key path get/setter are emitted only when resilient.
  // key path getter for UfiPkgClass.data : UfiPkgClass
  // UTILS-RES-DAG: sil shared [thunk] [ossa] @$s5Utils11UfiPkgClassC4dataSivpACTK : $@convention(keypath_accessor_getter) (@in_guaranteed UfiPkgClass) -> @out Int {

  // key path setter for UfiPkgClass.data : UfiPkgClass
  // UTILS-RES-DAG: sil shared [thunk] [ossa] @$s5Utils11UfiPkgClassC4dataSivpACTk : $@convention(keypath_accessor_setter) (@in_guaranteed Int, @in_guaranteed UfiPkgClass) -> () {

  // UfiPkgClass.data.getter
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils11UfiPkgClassC4dataSivg : $@convention(method) (@guaranteed UfiPkgClass) -> Int {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils11UfiPkgClassC4dataSivg : $@convention(method) (@guaranteed UfiPkgClass) -> Int {

  // UfiPkgClass.data.setter
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils11UfiPkgClassC4dataSivs : $@convention(method) (Int, @guaranteed UfiPkgClass) -> () {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils11UfiPkgClassC4dataSivs : $@convention(method) (Int, @guaranteed UfiPkgClass) -> () {

  // UfiPkgClass.data.modify
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils11UfiPkgClassC4dataSivM : $@yield_once @convention(method) (@guaranteed UfiPkgClass) -> @yields @inout Int {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils11UfiPkgClassC4dataSivM : $@yield_once @convention(method) (@guaranteed UfiPkgClass) -> @yields @inout Int {
  package var data: Int = 0

  // UfiPkgClass.__allocating_init()
  // UTILS-COMMON-DAG: sil package [exact_self_class] [ossa] @$s5Utils11UfiPkgClassCACycfC : $@convention(method) (@thick UfiPkgClass.Type) -> @owned UfiPkgClass {

  // UfiPkgClass.init()
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils11UfiPkgClassCACycfc : $@convention(method) (@owned UfiPkgClass) -> @owned UfiPkgClass {

  // UfiPkgClass.deinit
  // UTILS-COMMON-DAG: sil [ossa] @$s5Utils11UfiPkgClassCfd : $@convention(method) (@guaranteed UfiPkgClass) -> @owned Builtin.NativeObject {

  // UfiPkgClass.__deallocating_deinit
  // UTILS-COMMON-DAG: sil [ossa] @$s5Utils11UfiPkgClassCfD : $@convention(method) (@owned UfiPkgClass) -> () {
  package init() {}
}

@usableFromInline
class UfiInternalClass {
  // variable initialization expression of UfiInternalClass.data
  // UTILS-RES-DAG: sil hidden [transparent] [ossa] @$s5Utils16UfiInternalClassC4dataSivpfi : $@convention(thin) () ->
  // UTILS-NONRES-DAG: sil [transparent] [ossa] @$s5Utils16UfiInternalClassC4dataSivpfi : $@convention(thin) () ->

  // UfiInternalClass.data.getter
  // UTILS-RES-DAG: sil hidden [ossa] @$s5Utils16UfiInternalClassC4dataSivg : $@convention(method) (@guaranteed UfiInternalClass) -> Int {
  // UTILS-NONRES-DAG: sil hidden [transparent] [ossa] @$s5Utils16UfiInternalClassC4dataSivg : $@convention(method) (@guaranteed UfiInternalClass) -> Int {

  // UfiInternalClass.data.setter
  // UTILS-RES-DAG: sil hidden [ossa] @$s5Utils16UfiInternalClassC4dataSivs : $@convention(method) (Int, @guaranteed UfiInternalClass) -> () {
  // UTILS-NONRES-DAG: sil hidden [transparent] [ossa] @$s5Utils16UfiInternalClassC4dataSivs : $@convention(method) (Int, @guaranteed UfiInternalClass) -> () {

  // UfiInternalClass.data.modify
  // UTILS-RES-DAG:  sil hidden [ossa] @$s5Utils16UfiInternalClassC4dataSivM : $@yield_once @convention(method) (@guaranteed UfiInternalClass) -> @yields @inout Int {
  // UTILS-NONRES-DAG:  sil hidden [transparent] [ossa] @$s5Utils16UfiInternalClassC4dataSivM : $@yield_once @convention(method) (@guaranteed UfiInternalClass) -> @yields @inout Int {
  var data: Int = 0

  // UfiInternalClass.deinit
  // UTILS-COMMON-DAG: sil [ossa] @$s5Utils16UfiInternalClassCfd : $@convention(method) (@guaranteed UfiInternalClass) ->
  // UfiInternalClass.__deallocating_deinit
  // UTILS-COMMON-DAG: sil [ossa] @$s5Utils16UfiInternalClassCfD : $@convention(method) (@owned UfiInternalClass) -> () {
  // UfiInternalClass.__allocating_init()
  // UTILS-COMMON-DAG: sil hidden [exact_self_class] [ossa] @$s5Utils16UfiInternalClassCACycfC : $@convention(method) (@thick UfiInternalClass.Type) -> @owned UfiInternalClass {
  // UfiInternalClass.init()
  // UTILS-COMMON-DAG: sil hidden [ossa] @$s5Utils16UfiInternalClassCACycfc : $@convention(method) (@owned UfiInternalClass) -> @owned UfiInternalClass {
  init() {}
}

public struct PublicStruct {
  // variable initialization expression of PublicStruct.data
  // UTILS-RES-DAG: sil hidden [transparent] [ossa] @$s5Utils12PublicStructV4dataSivpfi : $@convention(thin) () -> Int {
  // UTILS-NONRES-DAG: sil [transparent] [ossa] @$s5Utils12PublicStructV4dataSivpfi : $@convention(thin) () -> Int {

  // PublicStruct.data.getter
  // UTILS-RES-DAG: sil [ossa] @$s5Utils12PublicStructV4dataSivg : $@convention(method) (@in_guaranteed PublicStruct) -> Int {
  // UTILS-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s5Utils12PublicStructV4dataSivg : $@convention(method) (PublicStruct) -> Int {

  // PublicStruct.data.setter
  // UTILS-RES-DAG: sil [ossa] @$s5Utils12PublicStructV4dataSivs : $@convention(method) (Int, @inout PublicStruct) -> () {
  // UTILS-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s5Utils12PublicStructV4dataSivs : $@convention(method) (Int, @inout PublicStruct) -> () {

  // PublicStruct.data.modify
  // UTILS-RES-DAG: sil [ossa] @$s5Utils12PublicStructV4dataSivM : $@yield_once @convention(method) (@inout PublicStruct) -> @yields @inout Int {
  // UTILS-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s5Utils12PublicStructV4dataSivM : $@yield_once @convention(method) (@inout PublicStruct) -> @yields @inout Int {
  public var data: Int = 0

  // PublicStruct.init()
  // UTILS-RES-DAG: sil [ossa] @$s5Utils12PublicStructVACycfC : $@convention(method) (@thin PublicStruct.Type) -> @out PublicStruct {
  // UTILS-NONRES-DAG: sil [ossa] @$s5Utils12PublicStructVACycfC : $@convention(method) (@thin PublicStruct.Type) -> PublicStruct {
  public init() {}
}

@_frozen
public struct FrozenPublicStruct {
  // variable initialization expression of FrozenPublicStruct.data
  // UTILS-COMMON-DAG: sil non_abi [transparent] [serialized] [ossa] @$s5Utils18FrozenPublicStructV4dataSivpfi : $@convention(thin) () -> Int {

  // FrozenPublicStruct.data.getter
  // UTILS-COMMON-DAG: sil [transparent] [serialized] [ossa] @$s5Utils18FrozenPublicStructV4dataSivg : $@convention(method) (FrozenPublicStruct) -> Int {

  // FrozenPublicStruct.data.setter
  // UTILS-COMMON-DAG: sil [transparent] [serialized] [ossa] @$s5Utils18FrozenPublicStructV4dataSivs : $@convention(method) (Int, @inout FrozenPublicStruct) -> () {

  // FrozenPublicStruct.data.modify
  // UTILS-COMMON-DAG: sil [transparent] [serialized] [ossa] @$s5Utils18FrozenPublicStructV4dataSivM : $@yield_once @convention(method) (@inout FrozenPublicStruct) -> @yields @inout Int {
  public var data: Int = 0

  // FrozenPublicStruct.init()
  // UTILS-COMMON-DAG: sil [ossa] @$s5Utils18FrozenPublicStructVACycfC : $@convention(method) (@thin FrozenPublicStruct.Type) -> FrozenPublicStruct {
  public init() {}
}

package struct PkgStruct {
  // variable initialization expression of PkgStruct.data
  // UTILS-RES-DAG: sil hidden [transparent] [ossa] @$s5Utils9PkgStructV4dataSivpfi : $@convention(thin) () -> Int {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils9PkgStructV4dataSivpfi : $@convention(thin) () -> Int {
  package var data: Int = 0
  // PkgStruct.init()
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils9PkgStructVACycfC : $@convention(method) (@thin PkgStruct.Type) -> @out PkgStruct {
  // UTILS-NONRES-DAG: sil package [ossa] @$s5Utils9PkgStructVACycfC : $@convention(method) (@thin PkgStruct.Type) -> PkgStruct {
  package init() {}
}

struct InternalStruct {
  // variable initialization expression of InternalStruct.data
  // UTILS-COMMON-DAG: sil hidden [transparent] [ossa] @$s5Utils14InternalStructV4dataSivpfi : $@convention(thin) () -> Int {
  var data: Int = 0
  // InternalStruct.init()
  // UTILS-COMMON-DAG: sil hidden [ossa] @$s5Utils14InternalStructVACycfC : $@convention(method) (@thin InternalStruct.Type) -> InternalStruct {
  init() {}
}

@usableFromInline
package struct UfiPkgStruct {
  // variable initialization expression of UfiPkgStruct.data
  // UTILS-RES-DAG: sil hidden [transparent] [ossa] @$s5Utils12UfiPkgStructV4dataSivpfi : $@convention(thin) () -> Int {
  // UTILS-NONRES-DAG: sil [transparent] [ossa] @$s5Utils12UfiPkgStructV4dataSivpfi : $@convention(thin) () -> Int {
  package var data: Int = 0
  // UfiPkgStruct.init()
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils12UfiPkgStructVACycfC : $@convention(method) (@thin UfiPkgStruct.Type) -> @out UfiPkgStruct {
  // UTILS-NONRES-DAG: sil package [ossa] @$s5Utils12UfiPkgStructVACycfC : $@convention(method) (@thin UfiPkgStruct.Type) -> UfiPkgStruct {
  package init() {}
}

@usableFromInline
struct UfiInternalStruct {
  // variable initialization expression of UfiInternalStruct.data
  // UTILS-RES-DAG: sil hidden [transparent] [ossa] @$s5Utils17UfiInternalStructV4dataSivpfi : $@convention(thin) () -> Int {
  // UTILS-NONRES-DAG: sil [transparent] [ossa] @$s5Utils17UfiInternalStructV4dataSivpfi : $@convention(thin) () -> Int {
  var data: Int = 0

  // UfiInternalStruct.init()
  // UTILS-RES-DAG: sil hidden [ossa] @$s5Utils17UfiInternalStructVACycfC : $@convention(method) (@thin UfiInternalStruct.Type) -> @out UfiInternalStruct {
  // UTILS-NONRES-DAG: sil hidden [ossa] @$s5Utils17UfiInternalStructVACycfC : $@convention(method) (@thin UfiInternalStruct.Type) -> UfiInternalStruct {
  init() {}
}

package struct PkgStructGeneric<T> {
  package var data: T
  package init(_ arg: T) { data = arg }
  // PkgStructGeneric.init(_:)
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils16PkgStructGenericVyACyxGxcfC : $@convention(method) <T> (@in T, @thin PkgStructGeneric<T>.Type) -> @out PkgStructGeneric<T> {
}

package class PkgClassGeneric<T> {
  package var data: T
  package init(_ arg: T) { data = arg }
  // key path getter for PkgClassGeneric.data : <A>PkgClassGeneric<A>
  // UTILS-RES-DAG: sil shared [thunk] [ossa] @$s5Utils15PkgClassGenericC4dataxvplACyxGTK : $@convention(keypath_accessor_getter) <T> (@in_guaranteed PkgClassGeneric<T>) -> @out T {

  // key path setter for PkgClassGeneric.data : <A>PkgClassGeneric<A>
  // UTILS-RES-DAG: sil shared [thunk] [ossa] @$s5Utils15PkgClassGenericC4dataxvplACyxGTk : $@convention(keypath_accessor_setter) <T> (@in_guaranteed T, @in_guaranteed PkgClassGeneric<T>) -> () {

  // PkgClassGeneric.data.getter
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils15PkgClassGenericC4dataxvg : $@convention(method) <T> (@guaranteed PkgClassGeneric<T>) -> @out T {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils15PkgClassGenericC4dataxvg : $@convention(method) <T> (@guaranteed PkgClassGeneric<T>) -> @out T {

  // PkgClassGeneric.data.setter
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils15PkgClassGenericC4dataxvs : $@convention(method) <T> (@in T, @guaranteed PkgClassGeneric<T>) -> () {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils15PkgClassGenericC4dataxvs : $@convention(method) <T> (@in T, @guaranteed PkgClassGeneric<T>) -> () {

  // PkgClassGeneric.data.modify
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils15PkgClassGenericC4dataxvM : $@yield_once @convention(method) <T> (@guaranteed PkgClassGeneric<T>) -> @yields @inout T {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils15PkgClassGenericC4dataxvM : $@yield_once @convention(method) <T> (@guaranteed PkgClassGeneric<T>) -> @yields @inout T {

  // PkgClassGeneric.__allocating_init(_:)
  // UTILS-COMMON-DAG: sil package [exact_self_class] [ossa] @$s5Utils15PkgClassGenericCyACyxGxcfC : $@convention(method) <T> (@in T, @thick PkgClassGeneric<T>.Type) -> @owned PkgClassGeneric<T> {

  // PkgClassGeneric.init(_:)
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils15PkgClassGenericCyACyxGxcfc : $@convention(method) <T> (@in T, @owned PkgClassGeneric<T>) -> @owned PkgClassGeneric<T> {
  // PkgClassGeneric.deinit
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils15PkgClassGenericCfd : $@convention(method) <T> (@guaranteed PkgClassGeneric<T>) -> @owned Builtin.NativeObject {
  // PkgClassGeneric.__deallocating_deinit
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils15PkgClassGenericCfD : $@convention(method) <T> (@owned PkgClassGeneric<T>) -> () {
}

package struct PkgStructWithPublicMember {
  package var member: PublicStruct
}

public struct PublicStructWithPublicExistential {
  // PublicStructWithPublicExistential.member.getter
  // UTILS-RES-DAG: sil [ossa] @$s5Utils016PublicStructWithB11ExistentialV6memberAA0B5Proto_pvg : $@convention(method) (@in_guaranteed PublicStructWithPublicExistential) -> @out any PublicProto {
  // UTILS-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s5Utils016PublicStructWithB11ExistentialV6memberAA0B5Proto_pvg : $@convention(method) (@in_guaranteed PublicStructWithPublicExistential) -> @out any PublicProto {

  // PublicStructWithPublicExistential.member.setter
  // UTILS-RES-DAG: sil [ossa] @$s5Utils016PublicStructWithB11ExistentialV6memberAA0B5Proto_pvs : $@convention(method) (@in any PublicProto, @inout PublicStructWithPublicExistential) -> () {
  // UTILS-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s5Utils016PublicStructWithB11ExistentialV6memberAA0B5Proto_pvs : $@convention(method) (@in any PublicProto, @inout PublicStructWithPublicExistential) -> () {

  // PublicStructWithPublicExistential.member.modify
  // UTILS-RES-DAG: sil [ossa] @$s5Utils016PublicStructWithB11ExistentialV6memberAA0B5Proto_pvM : $@yield_once @convention(method) (@inout PublicStructWithPublicExistential) -> @yields @inout any PublicProto {
  // UTILS-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s5Utils016PublicStructWithB11ExistentialV6memberAA0B5Proto_pvM : $@yield_once @convention(method) (@inout PublicStructWithPublicExistential) -> @yields @inout any PublicProto {
  public var member: any PublicProto

  // PublicStructWithPublicExistential.init(_:)
  // UTILS-COMMON-DAG: sil [ossa] @$s5Utils016PublicStructWithB11ExistentialVyAcA0B5Proto_pcfC : $@convention(method) (@in any PublicProto, @thin PublicStructWithPublicExistential.Type) -> @out PublicStructWithPublicExistential {
  public init(_ arg: any PublicProto) { member = arg }
}

package struct PkgStructWithPublicExistential {
  // PkgStructWithPublicExistential.member.getter
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils30PkgStructWithPublicExistentialV6memberAA0E5Proto_pvg : $@convention(method) (@in_guaranteed PkgStructWithPublicExistential) -> @out any PublicProto {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils30PkgStructWithPublicExistentialV6memberAA0E5Proto_pvg : $@convention(method) (@in_guaranteed PkgStructWithPublicExistential) -> @out any PublicProto {

  // PkgStructWithPublicExistential.member.setter
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils30PkgStructWithPublicExistentialV6memberAA0E5Proto_pvs : $@convention(method) (@in any PublicProto, @inout PkgStructWithPublicExistential) -> () {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils30PkgStructWithPublicExistentialV6memberAA0E5Proto_pvs : $@convention(method) (@in any PublicProto, @inout PkgStructWithPublicExistential) -> () {

  // PkgStructWithPublicExistential.member.modify
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils30PkgStructWithPublicExistentialV6memberAA0E5Proto_pvM : $@yield_once @convention(method) (@inout PkgStructWithPublicExistential) -> @yields @inout any PublicProto {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils30PkgStructWithPublicExistentialV6memberAA0E5Proto_pvM : $@yield_once @convention(method) (@inout PkgStructWithPublicExistential) -> @yields @inout any PublicProto {
  package var member: any PublicProto

  // PkgStructWithPublicExistential.init(_:)
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils30PkgStructWithPublicExistentialVyAcA0E5Proto_pcfC : $@convention(method) (@in any PublicProto, @thin PkgStructWithPublicExistential.Type) -> @out PkgStructWithPublicExistential {
  package init(_ arg: any PublicProto) { member = arg }
}

package struct PkgStructWithPkgExistential {
  // PkgStructWithPkgExistential.member.getter
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils013PkgStructWithB11ExistentialV6memberAA0B5Proto_pvg : $@convention(method) (@in_guaranteed PkgStructWithPkgExistential) -> @out any PkgProto {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils013PkgStructWithB11ExistentialV6memberAA0B5Proto_pvg : $@convention(method) (@in_guaranteed PkgStructWithPkgExistential) -> @out any PkgProto {

  // PkgStructWithPkgExistential.member.setter
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils013PkgStructWithB11ExistentialV6memberAA0B5Proto_pvs : $@convention(method) (@in any PkgProto, @inout PkgStructWithPkgExistential) -> () {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils013PkgStructWithB11ExistentialV6memberAA0B5Proto_pvs : $@convention(method) (@in any PkgProto, @inout PkgStructWithPkgExistential) -> () {

  // PkgStructWithPkgExistential.member.modify
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils013PkgStructWithB11ExistentialV6memberAA0B5Proto_pvM : $@yield_once @convention(method) (@inout PkgStructWithPkgExistential) -> @yields @inout any PkgProto {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils013PkgStructWithB11ExistentialV6memberAA0B5Proto_pvM : $@yield_once @convention(method) (@inout PkgStructWithPkgExistential) -> @yields @inout any PkgProto {
  package var member: any PkgProto

  // PkgStructWithPkgExistential.init(_:)
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils013PkgStructWithB11ExistentialVyAcA0B5Proto_pcfC : $@convention(method) (@in any PkgProto, @thin PkgStructWithPkgExistential.Type) -> @out PkgStructWithPkgExistential {
  package init(_ arg: any PkgProto) { member = arg }
}

// UTILS-RES-DAG: sil_vtable PublicKlass {
// UTILS-NONRES-DAG: sil_vtable [serialized] PublicKlass {

// FIXME: rdar://121976014 need to serialize package if non-resiliently built
// UTILS-COMMON-DAG: sil_vtable PkgKlass {
// UTILS-COMMON-DAG: sil_vtable PkgClassGeneric {

// UTILS-COMMON-DAG: sil_vtable InternalClass {

// UTILS-RES-DAG: sil_vtable UfiPkgClass {
// UTILS-NONRES-DAG: sil_vtable [serialized] UfiPkgClass {

// UTILS-RES-DAG: sil_vtable UfiInternalClass {
// UTILS-NONRES-DAG: sil_vtable [serialized] UfiInternalClass {

// UTILS-RES-DAG: sil_witness_table PublicKlass: PublicProto module Utils {
// UTILS-NONRES-DAG: sil_witness_table [serialized] PublicKlass: PublicProto module Utils {

// NOTE: package added below
// FIXME: rdar://121976014 serialize package when non-resilient
// UTILS-COMMON-DAG: sil_witness_table package PkgKlass: PkgProto module Utils {

// default witness table is emitted only when resiliently built
// UTILS-RES-DAG: sil_default_witness_table PublicProto {
// NOTE: package added below
// UTILS-RES-DAG: sil_default_witness_table package PkgProto {

// UTILS-RES-DAG:  sil_property #PublicKlass.data (settable_property $Int,  id #PublicKlass.data!getter : (PublicKlass) -> () -> Int, getter @$s5Utils11PublicKlassC4dataSivpACTK : $@convention(keypath_accessor_getter) (@in_guaranteed PublicKlass) -> @out Int, setter @$s5Utils11PublicKlassC4dataSivpACTk : $@convention(keypath_accessor_setter) (@in_guaranteed Int, @in_guaranteed PublicKlass) -> ())
// UTILS-NONRES-DAG: sil_property #PublicKlass.data ()

// UTILS-RES-DAG: sil_property #UfiPkgClass.data (settable_property $Int,  id #UfiPkgClass.data!getter : (UfiPkgClass) -> () -> Int, getter @$s5Utils11UfiPkgClassC4dataSivpACTK : $@convention(keypath_accessor_getter) (@in_guaranteed UfiPkgClass) -> @out Int, setter @$s5Utils11UfiPkgClassC4dataSivpACTk : $@convention(keypath_accessor_setter) (@in_guaranteed Int, @in_guaranteed UfiPkgClass) -> ())
// UTILS-NONRES-DAG: sil_property #UfiPkgClass.data ()

// UTILS-RES-DAG: sil_property #PkgKlass.data (settable_property $Int,  id #PkgKlass.data!getter : (PkgKlass) -> () -> Int, getter @$s5Utils8PkgKlassC4dataSivpACTK : $@convention(keypath_accessor_getter) (@in_guaranteed PkgKlass) -> @out Int, setter @$s5Utils8PkgKlassC4dataSivpACTk : $@convention(keypath_accessor_setter) (@in_guaranteed Int, @in_guaranteed PkgKlass) -> ())
// UTILS-NONRES-DAG: sil_property #PkgKlass.data ()

// UTILS-RES-DAG: sil_property #PkgClassGeneric.data<τ_0_0> (settable_property $τ_0_0,  id #PkgClassGeneric.data!getter : <T> (PkgClassGeneric<T>) -> () -> T, getter @$s5Utils15PkgClassGenericC4dataxvplACyxGTK : $@convention(keypath_accessor_getter) <τ_0_0> (@in_guaranteed PkgClassGeneric<τ_0_0>) -> @out τ_0_0, setter @$s5Utils15PkgClassGenericC4dataxvplACyxGTk : $@convention(keypath_accessor_setter) <τ_0_0> (@in_guaranteed τ_0_0, @in_guaranteed PkgClassGeneric<τ_0_0>) -> ())
// UTILS-NONRES-DAG: sil_property #PkgClassGeneric.data<τ_0_0> ()

// UTILS-RES-DAG: sil_property #PublicStruct.data (stored_property #PublicStruct.data : $Int)
// UTILS-NONRES-DAG: sil_property #PublicStruct.data ()

// UTILS-RES-DAG: sil_property #FrozenPublicStruct.data (stored_property #FrozenPublicStruct.data : $Int)
// UTILS-NONRES-DAG: sil_property #FrozenPublicStruct.data ()

// UTILS-RES-DAG: sil_property #PkgStruct.data (stored_property #PkgStruct.data : $Int)
// UTILS-NONRES-DAG: sil_property #PkgStruct.data ()

// UTILS-RES-DAG: sil_property #UfiPkgStruct.data (stored_property #UfiPkgStruct.data : $Int)
// UTILS-NONRES-DAG: sil_property #UfiPkgStruct.data ()

// UTILS-RES-DAG: sil_property #PkgStructGeneric.data<τ_0_0> (stored_property #PkgStructGeneric.data : $τ_0_0)
// UTILS-NONRES-DAG: sil_property #PkgStructGeneric.data<τ_0_0> ()

// UTILS-RES-DAG: sil_property #PkgStructWithPublicMember.member (stored_property #PkgStructWithPublicMember.member : $PublicStruct)
// UTILS-NONRES-DAG: sil_property #PkgStructWithPublicMember.member ()

// UTILS-RES-DAG: sil_property #PublicStructWithPublicExistential.member (stored_property #PublicStructWithPublicExistential.member : $any PublicProto)
// UTILS-NONRES-DAG: sil_property #PublicStructWithPublicExistential.member ()

// UTILS-RES-DAG: sil_property #PkgStructWithPublicExistential.member (stored_property #PkgStructWithPublicExistential.member : $any PublicProto)
// UTILS-NONRES-DAG: sil_property #PkgStructWithPublicExistential.member ()

// UTILS-RES-DAG: sil_property #PkgStructWithPkgExistential.member (stored_property #PkgStructWithPkgExistential.member : $any PkgProto)
// UTILS-NONRES-DAG: sil_property #PkgStructWithPkgExistential.member ()


//--- Client.swift
import Utils

package func f(_ arg: PublicStruct) -> Int {
  return arg.data
}

// CLIENT-RES-LABEL: // f(_:)
// CLIENT-RES-NEXT: // Isolation: unspecified
// CLIENT-RES-NEXT: sil package [ossa] @$s6Client1fySi5Utils12PublicStructVF : $@convention(thin) (@in_guaranteed PublicStruct) -> Int
// CLIENT-RES-LABEL: // PublicStruct.data.getter
// CLIENT-RES-NEXT: // Isolation: unspecified
// CLIENT-RES-NEXT: sil @$s5Utils12PublicStructV4dataSivg : $@convention(method) (@in_guaranteed PublicStruct) -> Int

// CLIENT-NONRES-LABEL: // f(_:)
// CLIENT-NONRES-NEXT: // Isolation: unspecified
// CLIENT-NONRES-NEXT: sil package [ossa] @$s6Client1fySi5Utils12PublicStructVF : $@convention(thin) (PublicStruct) -> Int

public func ff(_ arg: PublicStruct) -> Int {
  return arg.data
}

// CLIENT-RES-LABEL: // ff(_:)
// CLIENT-RES-NEXT: // Isolation: unspecified
// CLIENT-RES-NEXT: sil [ossa] @$s6Client2ffySi5Utils12PublicStructVF : $@convention(thin) (@in_guaranteed PublicStruct) -> Int

// CLIENT-NONRES-LABEL: // ff(_:)
// CLIENT-NONRES-NEXT: // Isolation: unspecified
// CLIENT-NONRES-NEXT: sil [ossa] @$s6Client2ffySi5Utils12PublicStructVF : $@convention(thin) (PublicStruct) -> Int


public func fx(_ arg: FrozenPublicStruct) -> Int {
  return arg.data
}

// CLIENT-COMMON-LABEL: // fx(_:)
// CLIENT-COMMON-LABEL: sil [ossa] @$s6Client2fxySi5Utils18FrozenPublicStructVF : $@convention(thin) (FrozenPublicStruct) -> Int {
// CLIENT-COMMON-LABEL: // %0 "arg"
// CLIENT-COMMON-LABEL: bb0(%0 : $FrozenPublicStruct):
// CLIENT-COMMON-LABEL:   debug_value %0 : $FrozenPublicStruct, let, name "arg", argno 1
// CLIENT-COMMON-LABEL:   %2 = struct_extract %0 : $FrozenPublicStruct, #FrozenPublicStruct.data
// CLIENT-COMMON-LABEL:   return %2 : $Int
// CLIENT-COMMON-LABEL: } // end sil function '$s6Client2fxySi5Utils18FrozenPublicStructVF'

package func fy(_ arg: FrozenPublicStruct) -> Int {
  return arg.data
}

// CLIENT-COMMON-LABEL: // fy(_:)
// CLIENT-COMMON-LABEL: sil package [ossa] @$s6Client2fyySi5Utils18FrozenPublicStructVF : $@convention(thin) (FrozenPublicStruct) -> Int {
// CLIENT-COMMON-LABEL: // %0 "arg"
// CLIENT-COMMON-LABEL: bb0(%0 : $FrozenPublicStruct):
// CLIENT-COMMON-LABEL:   debug_value %0 : $FrozenPublicStruct, let, name "arg", argno 1
// CLIENT-COMMON-LABEL:   %2 = struct_extract %0 : $FrozenPublicStruct, #FrozenPublicStruct.data
// CLIENT-COMMON-LABEL:   return %2 : $Int
// CLIENT-COMMON-LABEL: } // end sil function '$s6Client2fyySi5Utils18FrozenPublicStructVF'

package func g(_ arg: PkgStruct) -> Int {
  return arg.data
}

// CLIENT-RES-LABEL: // g(_:)
// CLIENT-RES-NEXT: // Isolation: unspecified
// CLIENT-RES-NEXT: sil package [ossa] @$s6Client1gySi5Utils9PkgStructVF : $@convention(thin) (@in_guaranteed PkgStruct) -> Int
// CLIENT-RES-LABEL: // PkgStruct.data.getter
// CLIENT-RES-NEXT: // Isolation: unspecified
// CLIENT-RES-NEXT: sil package_external @$s5Utils9PkgStructV4dataSivg : $@convention(method) (@in_guaranteed PkgStruct) -> Int

// CLIENT-NONRES-LABEL: // g(_:)
// CLIENT-NONRES-NEXT: // Isolation: unspecified
// CLIENT-NONRES-NEXT: sil package [ossa] @$s6Client1gySi5Utils9PkgStructVF : $@convention(thin) (PkgStruct) -> Int

package func gx(_ arg: UfiPkgClass) -> Int {
  return arg.data
}

// CLIENT-COMMON-LABEL: // gx(_:)
// CLIENT-COMMON-NEXT: // Isolation: unspecified
// CLIENT-COMMON-NEXT: sil package [ossa] @$s6Client2gxySi5Utils11UfiPkgClassCF : $@convention(thin) (@guaranteed UfiPkgClass) -> Int {

package func m<T>(_ arg: PkgStructGeneric<T>) -> T {
  return arg.data
}

// CLIENT-RES-LABEL: // m<A>(_:)
// CLIENT-RES-NEXT: // Isolation: unspecified
// CLIENT-RES-NEXT: sil package [ossa] @$s6Client1myx5Utils16PkgStructGenericVyxGlF : $@convention(thin) <T> (@in_guaranteed PkgStructGeneric<T>) -> @out T {

// CLIENT-RES-LABEL: // PkgStructGeneric.data.getter
// CLIENT-RES-NEXT: // Isolation: unspecified
// CLIENT-RES-NEXT: sil package_external @$s5Utils16PkgStructGenericV4dataxvg : $@convention(method) <τ_0_0> (@in_guaranteed PkgStructGeneric<τ_0_0>) -> @out τ_0_0

// CLIENT-NONRES-LABEL: // m<A>(_:)
// CLIENT-NONRES-NEXT: // Isolation: unspecified
// CLIENT-NONRES-NEXT: sil package [ossa] @$s6Client1myx5Utils16PkgStructGenericVyxGlF : $@convention(thin) <T> (@in_guaranteed PkgStructGeneric<T>) -> @out T {


package func m<T>(_ arg: PkgClassGeneric<T>) -> T {
  return arg.data
}


package func n(_ arg: PkgStructWithPublicMember) -> Int {
  return arg.member.data
}

// CLIENT-RES-LABEL: // n(_:)
// CLIENT-RES-NEXT: // Isolation: unspecified
// CLIENT-RES-NEXT: sil package [ossa] @$s6Client1nySi5Utils25PkgStructWithPublicMemberVF : $@convention(thin) (@in_guaranteed PkgStructWithPublicMember) -> Int

// CLIENT-RES-LABEL: // PkgStructWithPublicMember.member.getter
// CLIENT-RES-NEXT: // Isolation: unspecified
// CLIENT-RES-NEXT: sil package_external @$s5Utils25PkgStructWithPublicMemberV6memberAA0eC0Vvg : $@convention(method) (@in_guaranteed PkgStructWithPublicMember) -> @out PublicStruct


// CLIENT-NONRES-LABEL: // n(_:)
// CLIENT-NONRES-NEXT: // Isolation: unspecified
// CLIENT-NONRES-NEXT: sil package [ossa] @$s6Client1nySi5Utils25PkgStructWithPublicMemberVF : $@convention(thin) (PkgStructWithPublicMember) -> Int

package func p(_ arg: PkgStructWithPublicExistential) -> any PublicProto {
  return arg.member
}

// CLIENT-RES-LABEL: // p(_:)
// CLIENT-RES-NEXT: // Isolation: unspecified
// CLIENT-RES-NEXT: sil package [ossa] @$s6Client1py5Utils11PublicProto_pAC013PkgStructWithC11ExistentialVF : $@convention(thin) (@in_guaranteed PkgStructWithPublicExistential) -> @out any PublicProto {

// CLIENT-RES-LABEL: // PkgStructWithPublicExistential.member.getter
// CLIENT-RES-NEXT: // Isolation: unspecified
// CLIENT-RES-NEXT: sil package_external @$s5Utils30PkgStructWithPublicExistentialV6memberAA0E5Proto_pvg : $@convention(method) (@in_guaranteed PkgStructWithPublicExistential) -> @out any PublicProto


// CLIENT-NONRES-LABEL: // p(_:)
// CLIENT-NONRES-NEXT: // Isolation: unspecified
// CLIENT-NONRES-NEXT: sil package [ossa] @$s6Client1py5Utils11PublicProto_pAC013PkgStructWithC11ExistentialVF : $@convention(thin) (@in_guaranteed PkgStructWithPublicExistential) -> @out any PublicProto {

package func q(_ arg: PkgStructWithPkgExistential) -> any PkgProto {
  return arg.member
}

// CLIENT-RES-LABEL: // q(_:)
// CLIENT-RES-NEXT: // Isolation: unspecified
// CLIENT-RES-NEXT: sil package [ossa] @$s6Client1qy5Utils8PkgProto_pAC0c10StructWithC11ExistentialVF : $@convention(thin) (@in_guaranteed PkgStructWithPkgExistential) -> @out any PkgProto {

// CLIENT-RES-LABEL: // PkgStructWithPkgExistential.member.getter
// CLIENT-RES-NEXT: // Isolation: unspecified
// CLIENT-RES-NEXT: sil package_external @$s5Utils013PkgStructWithB11ExistentialV6memberAA0B5Proto_pvg : $@convention(method) (@in_guaranteed PkgStructWithPkgExistential) -> @out any PkgProto


// CLIENT-NONRES-LABEL: // q(_:)
// CLIENT-NONRES-NEXT: // Isolation: unspecified
// CLIENT-NONRES-NEXT: sil package [ossa] @$s6Client1qy5Utils8PkgProto_pAC0c10StructWithC11ExistentialVF : $@convention(thin) (@in_guaranteed PkgStructWithPkgExistential) -> @out any PkgProto {

package func r(_ arg: PublicProto) -> Int {
  return arg.data
}

// CLIENT-COMMON-LABEL: // r(_:)
// CLIENT-COMMON-NEXT: // Isolation: unspecified
// CLIENT-COMMON-NEXT: sil package [ossa] @$s6Client1rySi5Utils11PublicProto_pF : $@convention(thin) (@in_guaranteed any PublicProto) -> Int {

package func s(_ arg: PkgProto) -> Int {
  return arg.data
}

// CLIENT-COMMON-LABEL: // s(_:)
// CLIENT-COMMON-NEXT: // Isolation: unspecified
// CLIENT-COMMON-NEXT: sil package [ossa] @$s6Client1sySi5Utils8PkgProto_pF : $@convention(thin) (@in_guaranteed any PkgProto) -> Int {

public func t(_ arg: any PublicProto) -> Int {
  return arg.pfunc(arg.data)
}
// CLIENT-COMMON-LABEL: // t(_:)
// CLIENT-COMMON-LABEL: sil [ossa] @$s6Client1tySi5Utils11PublicProto_pF : $@convention(thin) (@in_guaranteed any PublicProto) -> Int

public func u(_ arg: PublicKlass) -> Int {
  return arg.pfunc(arg.data)
}

// CLIENT-COMMON-LABEL: // u(_:)
// CLIENT-COMMON-LABEL: sil [ossa] @$s6Client1uySi5Utils11PublicKlassCF : $@convention(thin) (@guaranteed PublicKlass) -> Int

package func v(_ arg: any PkgProto) -> Int {
  return arg.pkgfunc(arg.data)
}

// CLIENT-COMMON-LABEL: // v(_:)
// CLIENT-COMMON-LABEL: sil package [ossa] @$s6Client1vySi5Utils8PkgProto_pF : $@convention(thin) (@in_guaranteed any PkgProto) -> Int

package func w(_ arg: PkgKlass) -> Int {
  return arg.pkgfunc(arg.data)
}

// CLIENT-COMMON-LABEL: // w(_:)
// CLIENT-COMMON-NEXT: // Isolation: unspecified
// CLIENT-COMMON-NEXT: sil package [ossa] @$s6Client1wySi5Utils8PkgKlassCF : $@convention(thin) (@guaranteed PkgKlass) -> Int
