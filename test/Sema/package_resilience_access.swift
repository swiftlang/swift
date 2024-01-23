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
// RUN: %target-swift-emit-silgen -emit-verbose-sil -enable-library-evolution -module-name Utils %t/Utils.swift -package-name mypkg -I %t > %t/Utils-Res.sil
// RUN: %FileCheck %s --check-prefixes=UTILS-RES,UTILS-COMMON < %t/Utils-Res.sil

/// Check for indirect access with a resiliently built module dependency.
// RUN: %target-swift-emit-silgen %t/Client.swift -package-name mypkg -I %t > %t/Client-Res.sil
// RUN: %FileCheck %s --check-prefixes=CLIENT-RES,CLIENT-COMMON < %t/Client-Res.sil

// RUN: rm -rf %t/Utils.swiftmodule

/// Build Utils module non-resiliently
// RUN: %target-swift-frontend -emit-module %t/Utils.swift \
// RUN:   -module-name Utils -swift-version 5 -I %t \
// RUN:   -package-name mypkg \
// RUN:   -emit-module -emit-module-path %t/Utils.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t -swift-version 5 -package-name mypkg -verify

/// Check serialization in SILGEN with resilience not enabled.
// RUN: %target-swift-emit-silgen -emit-verbose-sil -module-name Utils %t/Utils.swift -package-name mypkg -I %t > %t/Utils-NonRes.sil
// RUN: %FileCheck %s --check-prefixes=UTILS-NONRES,UTILS-COMMON < %t/Utils-NonRes.sil

/// Check for indirect access with a non-resiliently built module dependency.
// RUN: %target-swift-emit-silgen %t/Client.swift -package-name mypkg -I %t > %t/Client-NonRes.sil
// RUN: %FileCheck %s --check-prefixes=CLIENT-NONRES,CLIENT-COMMON < %t/Client-NonRes.sil


//--- Utils.swift

public protocol PublicProto {
  var data: Int { get set }
  func pfunc(_ arg: Int) -> Int
}

public class PublicKlass: PublicProto {
    public var data: Int
    public init(data: Int = 1) {
        self.data = data
    }
    public func pfunc(_ arg: Int) -> Int {
        return data + arg
    }
}

// UTILS-RES-LABEL: // PublicKlass.data.getter
// UTILS-RES-NEXT: sil [ossa] @$s5Utils11PublicKlassC4dataSivg : $@convention(method) (@guaranteed PublicKlass) -> Int

// UTILS-NONRES-LABEL: // PublicKlass.data.getter
// UTILS-NONRES-NEXT: sil [transparent] [serialized] [ossa] @$s5Utils11PublicKlassC4dataSivg : $@convention(method) (@guaranteed PublicKlass) -> Int {

// UTILS-RES-LABEL: // PublicKlass.data.setter
// UTILS-RES-NEXT: sil [ossa] @$s5Utils11PublicKlassC4dataSivs : $@convention(method) (Int, @guaranteed PublicKlass) -> () {

// UTILS-NONRES-LABEL: // PublicKlass.data.setter
// UTILS-NONRES-NEXT: sil [transparent] [serialized] [ossa] @$s5Utils11PublicKlassC4dataSivs : $@convention(method) (Int, @guaranteed PublicKlass) -> () {

// UTILS-RES-LABEL: // PublicKlass.data.modify
// UTILS-RES-NEXT: sil [ossa] @$s5Utils11PublicKlassC4dataSivM : $@yield_once @convention(method) (@guaranteed PublicKlass) -> @yields @inout Int {

// UTILS-NONRES-LABEL: // PublicKlass.data.modify
// UTILS-NONRES-NEXT: sil [transparent] [serialized] [ossa] @$s5Utils11PublicKlassC4dataSivM : $@yield_once @convention(method) (@guaranteed PublicKlass) -> @yields @inout Int {

// UTILS-COMMON-LABEL: // default argument 0 of PublicKlass.init(data:)
// UTILS-COMMON-NEXT: sil non_abi [serialized] [ossa] @$s5Utils11PublicKlassC4dataACSi_tcfcfA_ : $@convention(thin) () -> Int {

// UTILS-COMMON-LABEL: // PublicKlass.__allocating_init(data:)
// UTILS-COMMON-NEXT: sil [serialized] [exact_self_class] [ossa] @$s5Utils11PublicKlassC4dataACSi_tcfC : $@convention(method) (Int, @thick PublicKlass.Type) -> @owned PublicKlass {

// UTILS-RES-LABEL: // protocol witness for PublicProto.data.getter in conformance PublicKlass
// UTILS-RES-NEXT: sil private [transparent] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP4dataSivgTW : $@convention(witness_method: PublicProto) (@in_guaranteed PublicKlass) -> Int {

// UTILS-NONRES-LABEL: // protocol witness for PublicProto.data.getter in conformance PublicKlass
// UTILS-NONRES-NEXT: sil shared [transparent] [serialized] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP4dataSivgTW : $@convention(witness_method: PublicProto) (@in_guaranteed PublicKlass) -> Int {

// UTILS-RES-LABEL: // protocol witness for PublicProto.data.setter in conformance PublicKlass
// UTILS-RES-NEXT: sil private [transparent] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP4dataSivsTW : $@convention(witness_method: PublicProto) (Int, @inout PublicKlass) -> () {

// UTILS-NONRES-LABEL: // protocol witness for PublicProto.data.setter in conformance PublicKlass
// UTILS-NONRES-NEXT: sil shared [transparent] [serialized] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP4dataSivsTW : $@convention(witness_method: PublicProto) (Int, @inout PublicKlass) -> () {


// UTILS-RES-LABEL: // protocol witness for PublicProto.data.modify in conformance PublicKlass
// UTILS-RES-NEXT: sil private [transparent] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP4dataSivMTW : $@yield_once @convention(witness_method: PublicProto) @substituted <τ_0_0> (@inout τ_0_0) -> @yields @inout Int for <PublicKlass> {

// UTILS-NONRES-LABEL: // protocol witness for PublicProto.data.modify in conformance PublicKlass
// UTILS-NONRES-NEXT: sil shared [transparent] [serialized] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP4dataSivMTW : $@yield_once @convention(witness_method: PublicProto) @substituted <τ_0_0> (@inout τ_0_0) -> @yields @inout Int for <PublicKlass> {

// UTILS-RES-LABEL: // protocol witness for PublicProto.pfunc(_:) in conformance PublicKlass
// UTILS-RES-NEXT: sil private [transparent] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP5pfuncyS2iFTW : $@convention(witness_method: PublicProto) (Int, @in_guaranteed PublicKlass) -> Int {

// UTILS-NONRES-LABEL: // protocol witness for PublicProto.pfunc(_:) in conformance PublicKlass
// UTILS-NONRES-NEXT: sil shared [transparent] [serialized] [thunk] [ossa] @$s5Utils11PublicKlassCAA0B5ProtoA2aDP5pfuncyS2iFTW : $@convention(witness_method: PublicProto) (Int, @in_guaranteed PublicKlass) -> Int {

package protocol PkgProto {
  var data: Int { get set }
  func pkgfunc(_ arg: Int) -> Int
}

package class PkgKlass: PkgProto {
    package var data: Int
    package init(data: Int = 1) {
        self.data = data
    }
    package func pkgfunc(_ arg: Int) -> Int {
        return data + arg
    }
}

// UTILS-COMMON-LABEL: // key path getter for PkgKlass.data : PkgKlass
// UTILS-COMMON-NEXT: sil shared [thunk] [ossa] @$s5Utils8PkgKlassC4dataSivpACTK : $@convention(keypath_accessor_getter) (@in_guaranteed PkgKlass) -> @out Int {

// UTILS-COMMON-LABEL: // key path setter for PkgKlass.data : PkgKlass
// UTILS-COMMON-NEXT: sil shared [thunk] [ossa] @$s5Utils8PkgKlassC4dataSivpACTk : $@convention(keypath_accessor_setter) (@in_guaranteed Int, @in_guaranteed PkgKlass) -> () {

// UTILS-RES-LABEL: // PkgKlass.data.getter
// UTILS-RES-NEXT: sil [ossa] @$s5Utils8PkgKlassC4dataSivg : $@convention(method) (@guaranteed PkgKlass) -> Int {
// UTILS-NONRES-LABEL: // PkgKlass.data.getter
// UTILS-NONRES-NEXT: sil [transparent] [ossa] @$s5Utils8PkgKlassC4dataSivg : $@convention(method) (@guaranteed PkgKlass) -> Int {

// UTILS-RES-LABEL: // PkgKlass.data.setter
// UTILS-RES-NEXT: sil [ossa] @$s5Utils8PkgKlassC4dataSivs : $@convention(method) (Int, @guaranteed PkgKlass) -> () {
// UTILS-NONRES-LABEL: // PkgKlass.data.setter
// UTILS-NONRES-NEXT: sil [transparent] [ossa] @$s5Utils8PkgKlassC4dataSivs : $@convention(method) (Int, @guaranteed PkgKlass) -> () {


// UTILS-RES-LABEL: // PkgKlass.data.modify
// UTILS-RES-NEXT: sil [ossa] @$s5Utils8PkgKlassC4dataSivM : $@yield_once @convention(method) (@guaranteed PkgKlass) -> @yields @inout Int {
// UTILS-NONRES-LABEL: // PkgKlass.data.modify
// UTILS-NONRES-NEXT: sil [transparent] [ossa] @$s5Utils8PkgKlassC4dataSivM : $@yield_once @convention(method) (@guaranteed PkgKlass) -> @yields @inout Int {

// UTILS-COMMON-LABEL: // default argument 0 of PkgKlass.init(data:)
// UTILS-COMMON-NEXT: sil [ossa] @$s5Utils8PkgKlassC4dataACSi_tcfcfA_ : $@convention(thin) () -> Int {

// UTILS-COMMON-LABEL: // PkgKlass.__allocating_init(data:)
// UTILS-COMMON-NEXT: sil [exact_self_class] [ossa] @$s5Utils8PkgKlassC4dataACSi_tcfC : $@convention(method) (Int, @thick PkgKlass.Type) -> @owned PkgKlass {

// UTILS-COMMON-LABEL: // PkgKlass.init(data:)
// UTILS-COMMON-NEXT: sil [ossa] @$s5Utils8PkgKlassC4dataACSi_tcfc : $@convention(method) (Int, @owned PkgKlass) -> @owned PkgKlass {

// UTILS-COMMON-LABEL: // PkgKlass.pkgfunc(_:)
// UTILS-COMMON-NEXT: sil [ossa] @$s5Utils8PkgKlassC7pkgfuncyS2iF : $@convention(method) (Int, @guaranteed PkgKlass) -> Int {

// UTILS-COMMON-LABEL: // PkgKlass.deinit
// UTILS-COMMON-NEXT: sil [ossa] @$s5Utils8PkgKlassCfd : $@convention(method) (@guaranteed PkgKlass) -> @owned Builtin.NativeObject {

// UTILS-COMMON-LABEL: // PkgKlass.__deallocating_deinit
// UTILS-COMMON-NEXT: sil [ossa] @$s5Utils8PkgKlassCfD : $@convention(method) (@owned PkgKlass) -> () {

// UTILS-COMMON-LABEL: // protocol witness for PkgProto.data.getter in conformance PkgKlass
// UTILS-COMMON-NEXT: sil private [transparent] [thunk] [ossa] @$s5Utils8PkgKlassCAA0B5ProtoA2aDP4dataSivgTW : $@convention(witness_method: PkgProto) (@in_guaranteed PkgKlass) -> Int {

// UTILS-COMMON-LABEL: // protocol witness for PkgProto.data.setter in conformance PkgKlass
// UTILS-COMMON-NEXT: sil private [transparent] [thunk] [ossa] @$s5Utils8PkgKlassCAA0B5ProtoA2aDP4dataSivsTW : $@convention(witness_method: PkgProto) (Int, @inout PkgKlass) -> () {

// UTILS-COMMON-LABEL: // protocol witness for PkgProto.pkgfunc(_:) in conformance PkgKlass
// UTILS-COMMON-NEXT: sil private [transparent] [thunk] [ossa] @$s5Utils8PkgKlassCAA0B5ProtoA2aDP7pkgfuncyS2iFTW : $@convention(witness_method: PkgProto) (Int, @in_guaranteed PkgKlass) -> Int {

public struct PublicStruct {
  public var data: Int
}

// UTILS-RES-LABEL: // PublicStruct.data.getter
// UTILS-RES-NEXT: sil [ossa] @$s5Utils12PublicStructV4dataSivg : $@convention(method) (@in_guaranteed PublicStruct) -> Int {

// UTILS-NONRES-LABEL: // PublicStruct.data.getter
// UTILS-NONRES-NEXT: sil [transparent] [serialized] [ossa] @$s5Utils12PublicStructV4dataSivg : $@convention(method) (PublicStruct) -> Int {

// UTILS-RES-LABEL: // PublicStruct.data.setter
// UTILS-RES-NEXT: sil [ossa] @$s5Utils12PublicStructV4dataSivs : $@convention(method) (Int, @inout PublicStruct) -> () {

// UTILS-NONRES-LABEL: // PublicStruct.data.setter
// UTILS-NONRES-NEXT: sil [transparent] [serialized] [ossa] @$s5Utils12PublicStructV4dataSivs : $@convention(method) (Int, @inout PublicStruct) -> () {

// UTILS-RES-LABEL: // PublicStruct.data.modify
// UTILS-RES-NEXT: sil [ossa] @$s5Utils12PublicStructV4dataSivM : $@yield_once @convention(method) (@inout PublicStruct) -> @yields @inout Int {

// UTILS-NONRES-LABEL: // PublicStruct.data.modify
// UTILS-NONRES-NEXT: sil [transparent] [serialized] [ossa] @$s5Utils12PublicStructV4dataSivM : $@yield_once @convention(method) (@inout PublicStruct) -> @yields @inout Int {

@_frozen
public struct FrozenPublicStruct {
  public var data: Int
}

// UTILS-COMMON-LABEL: // FrozenPublicStruct.data.getter
// UTILS-COMMON-NEXT: sil [transparent] [serialized] [ossa] @$s5Utils18FrozenPublicStructV4dataSivg : $@convention(method) (FrozenPublicStruct) -> Int {

// UTILS-COMMON-LABEL: // FrozenPublicStruct.data.setter
// UTILS-COMMON-NEXT: sil [transparent] [serialized] [ossa] @$s5Utils18FrozenPublicStructV4dataSivs : $@convention(method) (Int, @inout FrozenPublicStruct) -> () {

// UTILS-COMMON-LABEL: // FrozenPublicStruct.data.modify
// UTILS-COMMON-NEXT: sil [transparent] [serialized] [ossa] @$s5Utils18FrozenPublicStructV4dataSivM : $@yield_once @convention(method) (@inout FrozenPublicStruct) -> @yields @inout Int {

package struct PkgStruct {
  package var data: Int
}

// UTILS-RES-LABEL: // PkgStruct.data.getter
// UTILS-RES-NEXT: sil [ossa] @$s5Utils9PkgStructV4dataSivg : $@convention(method) (@in_guaranteed PkgStruct) -> Int {

// UTILS-NONRES-LABEL: // PkgStruct.data.getter
// UTILS-NONRES-NEXT: sil [transparent] [ossa] @$s5Utils9PkgStructV4dataSivg : $@convention(method) (PkgStruct) -> Int {

// UTILS-RES-LABEL: // PkgStruct.data.setter
// UTILS-RES-NEXT: sil [ossa] @$s5Utils9PkgStructV4dataSivs : $@convention(method) (Int, @inout PkgStruct) -> () {

// UTILS-NONRES-LABEL: // PkgStruct.data.setter
// UTILS-NONRES-NEXT: sil [transparent] [ossa] @$s5Utils9PkgStructV4dataSivs : $@convention(method) (Int, @inout PkgStruct) -> () {

// UTILS-RES-LABEL: // PkgStruct.data.modify
// UTILS-RES-NEXT: sil [ossa] @$s5Utils9PkgStructV4dataSivM : $@yield_once @convention(method) (@inout PkgStruct) -> @yields @inout Int {

// UTILS-NONRES-LABEL: // PkgStruct.data.modify
// UTILS-NONRES-NEXT: sil [transparent] [ossa] @$s5Utils9PkgStructV4dataSivM : $@yield_once @convention(method) (@inout PkgStruct) -> @yields @inout Int {

@usableFromInline
package struct UfiPkgStruct {
  package var data: Int
}

// UTILS-RES-LABEL: // UfiPkgStruct.data.getter
// UTILS-RES-NEXT: sil [ossa] @$s5Utils12UfiPkgStructV4dataSivg : $@convention(method) (@in_guaranteed UfiPkgStruct) -> Int {

// UTILS-NONRES-LABEL: // UfiPkgStruct.data.getter
// UTILS-NONRES-NEXT: sil [transparent] [ossa] @$s5Utils12UfiPkgStructV4dataSivg : $@convention(method) (UfiPkgStruct) -> Int {

// UTILS-RES-LABEL: // UfiPkgStruct.data.setter
// UTILS-RES-NEXT: sil [ossa] @$s5Utils12UfiPkgStructV4dataSivs : $@convention(method) (Int, @inout UfiPkgStruct) -> () {

// UTILS-NONRES-LABEL: // UfiPkgStruct.data.setter
// UTILS-NONRES-NEXT: sil [transparent] [ossa] @$s5Utils12UfiPkgStructV4dataSivs : $@convention(method) (Int, @inout UfiPkgStruct) -> () {

// UTILS-RES-LABEL: // UfiPkgStruct.data.modify
// UTILS-RES-NEXT: sil [ossa] @$s5Utils12UfiPkgStructV4dataSivM : $@yield_once @convention(method) (@inout UfiPkgStruct) -> @yields @inout Int {

// UTILS-NONRES-LABEL: // UfiPkgStruct.data.modify
// UTILS-NONRES-NEXT: sil [transparent] [ossa] @$s5Utils12UfiPkgStructV4dataSivM : $@yield_once @convention(method) (@inout UfiPkgStruct) -> @yields @inout Int {

package struct PkgStructGeneric<T> {
  package var data: T
}

package struct PkgStructWithPublicMember {
  package var member: PublicStruct
}

package struct PkgStructWithPublicExistential {
  package var member: any PublicProto
}

package struct PkgStructWithPkgExistential {
  package var member: any PkgProto
}

struct InternalStruct {
    var data: Int
}

// UTILS-RES-LABEL: sil_vtable PublicKlass {
// UTILS-NONRES-LABEL: sil_vtable [serialized] PublicKlass {
// UTILS-COMMON-LABEL: sil_vtable PkgKlass {

// UTILS-RES-LABEL: sil_witness_table PublicKlass: PublicProto module Utils {
// UTILS-NONRES-LABEL: sil_witness_table [serialized] PublicKlass: PublicProto module Utils {
// UTILS-COMMON-LABEL: sil_witness_table PkgKlass: PkgProto module Utils {



//--- Client.swift
import Utils

package func f(_ arg: PublicStruct) -> Int {
  return arg.data
}

// CLIENT-RES-LABEL: // f(_:)
// CLIENT-RES-NEXT: sil [ossa] @$s6Client1fySi5Utils12PublicStructVF : $@convention(thin) (@in_guaranteed PublicStruct) -> Int
// CLIENT-RES-LABEL: // PublicStruct.data.getter
// CLIENT-RES-NEXT: sil @$s5Utils12PublicStructV4dataSivg : $@convention(method) (@in_guaranteed PublicStruct) -> Int

// CLIENT-NONRES-LABEL: // f(_:)
// CLIENT-NONRES-NEXT: sil [ossa] @$s6Client1fySi5Utils12PublicStructVF : $@convention(thin) (PublicStruct) -> Int

public func ff(_ arg: PublicStruct) -> Int {
  return arg.data
}

// CLIENT-RES-LABEL: // ff(_:)
// CLIENT-RES-NEXT: sil [ossa] @$s6Client2ffySi5Utils12PublicStructVF : $@convention(thin) (@in_guaranteed PublicStruct) -> Int

// CLIENT-NONRES-LABEL: // ff(_:)
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
// CLIENT-COMMON-LABEL: sil [ossa] @$s6Client2fyySi5Utils18FrozenPublicStructVF : $@convention(thin) (FrozenPublicStruct) -> Int {
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
// CLIENT-RES-NEXT: sil [ossa] @$s6Client1gySi5Utils9PkgStructVF : $@convention(thin) (@in_guaranteed PkgStruct) -> Int
// CLIENT-RES-LABEL: // PkgStruct.data.getter
// CLIENT-RES-NEXT: sil @$s5Utils9PkgStructV4dataSivg : $@convention(method) (@in_guaranteed PkgStruct) -> Int

// CLIENT-NONRES-LABEL: // g(_:)
// CLIENT-NONRES-NEXT: sil [ossa] @$s6Client1gySi5Utils9PkgStructVF : $@convention(thin) (PkgStruct) -> Int

package func gx(_ arg: UfiPkgStruct) -> Int {
  return arg.data
}

// CLIENT-RES-LABEL: // gx(_:)
// CLIENT-RES-NEXT: sil [ossa] @$s6Client2gxySi5Utils12UfiPkgStructVF : $@convention(thin) (@in_guaranteed UfiPkgStruct) -> Int
// CLIENT-RES-LABEL: // UfiPkgStruct.data.getter
// CLIENT-RES-NEXT: sil @$s5Utils12UfiPkgStructV4dataSivg : $@convention(method) (@in_guaranteed UfiPkgStruct) -> Int

// CLIENT-NONRES-LABEL: // gx(_:)
// CLIENT-NONRES-NEXT: sil [ossa] @$s6Client2gxySi5Utils12UfiPkgStructVF : $@convention(thin) (UfiPkgStruct) -> Int

package func m<T>(_ arg: PkgStructGeneric<T>) -> T {
  return arg.data
}

// CLIENT-RES-LABEL: // m<A>(_:)
// CLIENT-RES-NEXT: sil [ossa] @$s6Client1myx5Utils16PkgStructGenericVyxGlF : $@convention(thin) <T> (@in_guaranteed PkgStructGeneric<T>) -> @out T {
// CLIENT-RES-NEXT: // %0 "$return_value"
// CLIENT-RES-NEXT: // %1 "arg"
// CLIENT-RES-NEXT: bb0(%0 : $*T, %1 : $*PkgStructGeneric<T>):
// CLIENT-RES-NEXT:   debug_value %1 : $*PkgStructGeneric<T>, let, name "arg", argno 1, expr op_deref
// CLIENT-RES-NEXT:   %3 = alloc_stack $PkgStructGeneric<T>
// CLIENT-RES-NEXT:   copy_addr %1 to [init] %3 : $*PkgStructGeneric<T>
// CLIENT-RES-NEXT:   // function_ref PkgStructGeneric.data.getter
// CLIENT-RES-NEXT:   %5 = function_ref @$s5Utils16PkgStructGenericV4dataxvg : $@convention(method) <τ_0_0> (@in_guaranteed PkgStructGeneric<τ_0_0>) -> @out τ_0_0
// CLIENT-RES-NEXT:   %6 = apply %5<T>(%0, %3) : $@convention(method) <τ_0_0> (@in_guaranteed PkgStructGeneric<τ_0_0>) -> @out τ_0_0
// CLIENT-RES-NEXT:   destroy_addr %3 : $*PkgStructGeneric<T>
// CLIENT-RES-NEXT:   dealloc_stack %3 : $*PkgStructGeneric<T>
// CLIENT-RES-NEXT:   %9 = tuple ()
// CLIENT-RES-NEXT:   return %9 : $()
// CLIENT-RES-NEXT: } // end sil function '$s6Client1myx5Utils16PkgStructGenericVyxGlF'

// CLIENT-RES-LABEL: // PkgStructGeneric.data.getter
// CLIENT-RES-NEXT: sil @$s5Utils16PkgStructGenericV4dataxvg : $@convention(method) <τ_0_0> (@in_guaranteed PkgStructGeneric<τ_0_0>) -> @out τ_0_0

// CLIENT-NONRES-LABEL: // m<A>(_:)
// CLIENT-NONRES-NEXT: sil [ossa] @$s6Client1myx5Utils16PkgStructGenericVyxGlF : $@convention(thin) <T> (@in_guaranteed PkgStructGeneric<T>) -> @out T {
// CLIENT-NONRES-NEXT: // %0 "$return_value"
// CLIENT-NONRES-NEXT: // %1 "arg"
// CLIENT-NONRES-NEXT: bb0(%0 : $*T, %1 : $*PkgStructGeneric<T>):
// CLIENT-NONRES-NEXT:   debug_value %1 : $*PkgStructGeneric<T>, let, name "arg", argno 1, expr op_deref
// CLIENT-NONRES-NEXT:   %3 = struct_element_addr %1 : $*PkgStructGeneric<T>, #PkgStructGeneric.data
// CLIENT-NONRES-NEXT:   copy_addr %3 to [init] %0 : $*T
// CLIENT-NONRES-NEXT:   %5 = tuple ()
// CLIENT-NONRES-NEXT:   return %5 : $()
// CLIENT-NONRES-NEXT: } // end sil function '$s6Client1myx5Utils16PkgStructGenericVyxGlF'


package func n(_ arg: PkgStructWithPublicMember) -> Int {
  return arg.member.data
}

// CLIENT-RES-LABEL: // n(_:)
// CLIENT-RES-NEXT: sil [ossa] @$s6Client1nySi5Utils25PkgStructWithPublicMemberVF : $@convention(thin) (@in_guaranteed PkgStructWithPublicMember) -> Int
// CLIENT-RES-LABEL: // PkgStructWithPublicMember.member.getter
// CLIENT-RES-NEXT: sil @$s5Utils25PkgStructWithPublicMemberV6memberAA0eC0Vvg : $@convention(method) (@in_guaranteed PkgStructWithPublicMember) -> @out PublicStruct


// CLIENT-NONRES-LABEL: // n(_:)
// CLIENT-NONRES-NEXT: sil [ossa] @$s6Client1nySi5Utils25PkgStructWithPublicMemberVF : $@convention(thin) (PkgStructWithPublicMember) -> Int

package func p(_ arg: PkgStructWithPublicExistential) -> any PublicProto {
  return arg.member
}

// CLIENT-RES-LABEL: // p(_:)
// CLIENT-RES-NEXT: sil [ossa] @$s6Client1py5Utils11PublicProto_pAC013PkgStructWithC11ExistentialVF : $@convention(thin) (@in_guaranteed PkgStructWithPublicExistential) -> @out any PublicProto {
// CLIENT-RES-NEXT: // %0 "$return_value"
// CLIENT-RES-NEXT: // %1 "arg"
// CLIENT-RES-NEXT: bb0(%0 : $*any PublicProto, %1 : $*PkgStructWithPublicExistential):
// CLIENT-RES-NEXT:   debug_value %1 : $*PkgStructWithPublicExistential, let, name "arg", argno 1, expr op_deref
// CLIENT-RES-NEXT:   %3 = alloc_stack $PkgStructWithPublicExistential
// CLIENT-RES-NEXT:   copy_addr %1 to [init] %3 : $*PkgStructWithPublicExistential
// CLIENT-RES-NEXT:   // function_ref PkgStructWithPublicExistential.member.getter
// CLIENT-RES-NEXT:   %5 = function_ref @$s5Utils30PkgStructWithPublicExistentialV6memberAA0E5Proto_pvg : $@convention(method) (@in_guaranteed PkgStructWithPublicExistential) -> @out any PublicProto
// CLIENT-RES-NEXT:   %6 = apply %5(%0, %3) : $@convention(method) (@in_guaranteed PkgStructWithPublicExistential) -> @out any PublicProto
// CLIENT-RES-NEXT:   destroy_addr %3 : $*PkgStructWithPublicExistential
// CLIENT-RES-NEXT:   dealloc_stack %3 : $*PkgStructWithPublicExistential
// CLIENT-RES-NEXT:   %9 = tuple ()
// CLIENT-RES-NEXT:   return %9 : $()
// CLIENT-RES-NEXT: } // end sil function '$s6Client1py5Utils11PublicProto_pAC013PkgStructWithC11ExistentialVF'

// CLIENT-RES-LABEL: // PkgStructWithPublicExistential.member.getter
// CLIENT-RES-NEXT: sil @$s5Utils30PkgStructWithPublicExistentialV6memberAA0E5Proto_pvg : $@convention(method) (@in_guaranteed PkgStructWithPublicExistential) -> @out any PublicProto


// CLIENT-NONRES-LABEL: // p(_:)
// CLIENT-NONRES-NEXT: sil [ossa] @$s6Client1py5Utils11PublicProto_pAC013PkgStructWithC11ExistentialVF : $@convention(thin) (@in_guaranteed PkgStructWithPublicExistential) -> @out any PublicProto {
// CLIENT-NONRES-NEXT: // %0 "$return_value"
// CLIENT-NONRES-NEXT: // %1 "arg"
// CLIENT-NONRES-NEXT: bb0(%0 : $*any PublicProto, %1 : $*PkgStructWithPublicExistential):
// CLIENT-NONRES-NEXT:   debug_value %1 : $*PkgStructWithPublicExistential, let, name "arg", argno 1, expr op_deref
// CLIENT-NONRES-NEXT:   %3 = struct_element_addr %1 : $*PkgStructWithPublicExistential, #PkgStructWithPublicExistential.member
// CLIENT-NONRES-NEXT:   copy_addr %3 to [init] %0 : $*any PublicProto
// CLIENT-NONRES-NEXT:   %5 = tuple ()
// CLIENT-NONRES-NEXT:   return %5 : $()
// CLIENT-NONRES-NEXT: } // end sil function '$s6Client1py5Utils11PublicProto_pAC013PkgStructWithC11ExistentialVF'


package func q(_ arg: PkgStructWithPkgExistential) -> any PkgProto {
  return arg.member
}

// CLIENT-RES-LABEL: // q(_:)
// CLIENT-RES-NEXT: sil [ossa] @$s6Client1qy5Utils8PkgProto_pAC0c10StructWithC11ExistentialVF : $@convention(thin) (@in_guaranteed PkgStructWithPkgExistential) -> @out any PkgProto {
// CLIENT-RES-NEXT: // %0 "$return_value"
// CLIENT-RES-NEXT: // %1 "arg"
// CLIENT-RES-NEXT: bb0(%0 : $*any PkgProto, %1 : $*PkgStructWithPkgExistential):
// CLIENT-RES-NEXT:   debug_value %1 : $*PkgStructWithPkgExistential, let, name "arg", argno 1, expr op_deref
// CLIENT-RES-NEXT:   %3 = alloc_stack $PkgStructWithPkgExistential
// CLIENT-RES-NEXT:   copy_addr %1 to [init] %3 : $*PkgStructWithPkgExistential
// CLIENT-RES-NEXT:   // function_ref PkgStructWithPkgExistential.member.getter
// CLIENT-RES-NEXT:   %5 = function_ref @$s5Utils013PkgStructWithB11ExistentialV6memberAA0B5Proto_pvg : $@convention(method) (@in_guaranteed PkgStructWithPkgExistential) -> @out any PkgProto
// CLIENT-RES-NEXT:   %6 = apply %5(%0, %3) : $@convention(method) (@in_guaranteed PkgStructWithPkgExistential) -> @out any PkgProto
// CLIENT-RES-NEXT:   destroy_addr %3 : $*PkgStructWithPkgExistential
// CLIENT-RES-NEXT:   dealloc_stack %3 : $*PkgStructWithPkgExistential
// CLIENT-RES-NEXT:   %9 = tuple ()
// CLIENT-RES-NEXT:   return %9 : $()
// CLIENT-RES-NEXT: } // end sil function '$s6Client1qy5Utils8PkgProto_pAC0c10StructWithC11ExistentialVF'

// CLIENT-RES-LABEL: // PkgStructWithPkgExistential.member.getter
// CLIENT-RES-NEXT: sil @$s5Utils013PkgStructWithB11ExistentialV6memberAA0B5Proto_pvg : $@convention(method) (@in_guaranteed PkgStructWithPkgExistential) -> @out any PkgProto


// CLIENT-NONRES-LABEL: // q(_:)
// CLIENT-NONRES-NEXT: sil [ossa] @$s6Client1qy5Utils8PkgProto_pAC0c10StructWithC11ExistentialVF : $@convention(thin) (@in_guaranteed PkgStructWithPkgExistential) -> @out any PkgProto {
// CLIENT-NONRES-NEXT: // %0 "$return_value"
// CLIENT-NONRES-NEXT: // %1 "arg"
// CLIENT-NONRES-NEXT: bb0(%0 : $*any PkgProto, %1 : $*PkgStructWithPkgExistential):
// CLIENT-NONRES-NEXT:   debug_value %1 : $*PkgStructWithPkgExistential, let, name "arg", argno 1, expr op_deref
// CLIENT-NONRES-NEXT:   %3 = struct_element_addr %1 : $*PkgStructWithPkgExistential, #PkgStructWithPkgExistential.member
// CLIENT-NONRES-NEXT:   copy_addr %3 to [init] %0 : $*any PkgProto
// CLIENT-NONRES-NEXT:   %5 = tuple ()
// CLIENT-NONRES-NEXT:   return %5 : $()
// CLIENT-NONRES-NEXT: } // end sil function '$s6Client1qy5Utils8PkgProto_pAC0c10StructWithC11ExistentialVF'


package func r(_ arg: PublicProto) -> Int {
  return arg.data
}

// CLIENT-COMMON-LABEL: // r(_:)
// CLIENT-COMMON-NEXT: sil [ossa] @$s6Client1rySi5Utils11PublicProto_pF : $@convention(thin) (@in_guaranteed any PublicProto) -> Int {
// CLIENT-COMMON-NEXT: // %0 "arg"
// CLIENT-COMMON-NEXT: bb0(%0 : $*any PublicProto):
// CLIENT-COMMON-NEXT:   debug_value %0 : $*any PublicProto, let, name "arg", argno 1, expr op_deref
// CLIENT-COMMON-NEXT:   %2 = open_existential_addr immutable_access %0 : $*any PublicProto to $*@opened({{.*}}, any PublicProto) Self
// CLIENT-COMMON-NEXT:   %3 = alloc_stack $@opened("{{.*}}", any PublicProto) Self
// CLIENT-COMMON-NEXT:   copy_addr %2 to [init] %3 : $*@opened("{{.*}}", any PublicProto) Self
// CLIENT-COMMON-NEXT:   %5 = witness_method $@opened("{{.*}}", any PublicProto) Self, #PublicProto.data!getter : <Self where Self : Utils.PublicProto> (Self) -> () -> Int, %2 : $*@opened("{{.*}}", any PublicProto) Self : $@convention(witness_method: PublicProto) <τ_0_0 where τ_0_0 : PublicProto> (@in_guaranteed τ_0_0) -> Int
// CLIENT-COMMON-NEXT:   %6 = apply %5<@opened("{{.*}}", any PublicProto) Self>(%3) : $@convention(witness_method: PublicProto) <τ_0_0 where τ_0_0 : PublicProto> (@in_guaranteed τ_0_0) -> Int
// CLIENT-COMMON-NEXT:   destroy_addr %3 : $*@opened("{{.*}}", any PublicProto) Self
// CLIENT-COMMON-NEXT:   dealloc_stack %3 : $*@opened("{{.*}}", any PublicProto) Self
// CLIENT-COMMON-NEXT:   return %6 : $Int
// CLIENT-COMMON-NEXT: } // end sil function '$s6Client1rySi5Utils11PublicProto_pF'

package func s(_ arg: PkgProto) -> Int {
  return arg.data
}

// CLIENT-COMMON-LABEL: // s(_:)
// CLIENT-COMMON-NEXT: sil [ossa] @$s6Client1sySi5Utils8PkgProto_pF : $@convention(thin) (@in_guaranteed any PkgProto) -> Int {
// CLIENT-COMMON-NEXT: // %0 "arg"
// CLIENT-COMMON-NEXT: bb0(%0 : $*any PkgProto):
// CLIENT-COMMON-NEXT:   debug_value %0 : $*any PkgProto, let, name "arg", argno 1, expr op_deref
// CLIENT-COMMON-NEXT:   %2 = open_existential_addr immutable_access %0 : $*any PkgProto to $*@opened("{{.*}}", any PkgProto) Self
// CLIENT-COMMON-NEXT:   %3 = alloc_stack $@opened("{{.*}}", any PkgProto) Self
// CLIENT-COMMON-NEXT:   copy_addr %2 to [init] %3 : $*@opened("{{.*}}", any PkgProto) Self
// CLIENT-COMMON-NEXT:   %5 = witness_method $@opened("{{.*}}", any PkgProto) Self, #PkgProto.data!getter : <Self where Self : Utils.PkgProto> (Self) -> () -> Int, %2 : $*@opened("{{.*}}", any PkgProto) Self : $@convention(witness_method: PkgProto) <τ_0_0 where τ_0_0 : PkgProto> (@in_guaranteed τ_0_0) -> Int
// CLIENT-COMMON-NEXT:   %6 = apply %5<@opened("{{.*}}", any PkgProto) Self>(%3) : $@convention(witness_method: PkgProto) <τ_0_0 where τ_0_0 : PkgProto> (@in_guaranteed τ_0_0) -> Int
// CLIENT-COMMON-NEXT:   destroy_addr %3 : $*@opened("{{.*}}", any PkgProto) Self
// CLIENT-COMMON-NEXT:   dealloc_stack %3 : $*@opened("{{.*}}", any PkgProto) Self
// CLIENT-COMMON-NEXT:   return %6 : $Int
// CLIENT-COMMON-NEXT: } // end sil function '$s6Client1sySi5Utils8PkgProto_pF'


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
// CLIENT-COMMON-LABEL: sil [ossa] @$s6Client1vySi5Utils8PkgProto_pF : $@convention(thin) (@in_guaranteed any PkgProto) -> Int

package func w(_ arg: PkgKlass) -> Int {
   return arg.pkgfunc(arg.data)
}

// CLIENT-COMMON-LABEL: // w(_:)
// CLIENT-COMMON-NEXT: sil [ossa] @$s6Client1wySi5Utils8PkgKlassCF : $@convention(thin) (@guaranteed PkgKlass) -> Int
