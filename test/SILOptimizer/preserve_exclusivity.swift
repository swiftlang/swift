// RUN: %target-swift-frontend -parse-stdlib -Xllvm -sil-disable-pass=FunctionSignatureOpts -Xllvm -sil-disable-pass=GenericSpecializer -emit-sil -O %s | %FileCheck %s
//
// The -O pipeline should respect
// @_semantics("optimize.sil.preserve_exclusivity") and avoid eliminating access
// markers as long as the semantics call is not inlined.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-stdlib -emit-module -O -primary-file %s -emit-module-path %t/preserve_exclusivity.swiftmodule
// RUN: %target-sil-opt %t/preserve_exclusivity.swiftmodule -emit-sorted-sil -verify -o - | %FileCheck --check-prefix=DESERIALIZED %s
//
// Access markers cannot be stripped prior to module serialization, even when their functions are inlined.

@usableFromInline
@_silgen_name("marker1")
func marker1() -> ()

@usableFromInline
@_silgen_name("marker2")
func marker2() -> ()

@usableFromInline
@_silgen_name("marker3")
func marker3() -> ()

@usableFromInline
@_silgen_name("marker4")
func marker4() -> ()

@usableFromInline
@_silgen_name("marker5")
func marker5() -> ()

@usableFromInline
@_silgen_name("marker6")
func marker6() -> ()

// CHECK-LABEL: sil [_semantics "optimize.sil.preserve_exclusivity"] @$S20preserve_exclusivity13f1_beginNoOptyyBp_BpxmtlF : $@convention(thin) <T1> (Builtin.RawPointer, Builtin.RawPointer, @thick T1.Type) -> () {
// CHECK:   begin_unpaired_access
// CHECK: } // end sil function '$S20preserve_exclusivity13f1_beginNoOptyyBp_BpxmtlF'

// DESERIALIZED-LABEL: sil [serialized] [_semantics "optimize.sil.preserve_exclusivity"] [canonical] @$S20preserve_exclusivity13f1_beginNoOptyyBp_BpxmtlF : $@convention(thin) <T1> (Builtin.RawPointer, Builtin.RawPointer, @thick T1.Type) -> () {
// DESERIALIZED:   begin_unpaired_access
// DESERIALIZED: } // end sil function '$S20preserve_exclusivity13f1_beginNoOptyyBp_BpxmtlF'

@inlinable
@_semantics("optimize.sil.preserve_exclusivity")
public func f1_beginNoOpt<T1>(_ address: Builtin.RawPointer, _ scratch: Builtin.RawPointer, _ ty1: T1.Type) {
  marker1()
  Builtin.beginUnpairedModifyAccess(address, scratch, ty1);
}

// CHECK-LABEL: sil [_semantics "optimize.sil.preserve_exclusivity"] @$S20preserve_exclusivity13f2___endNoOptyyBpF : $@convention(thin) (Builtin.RawPointer) -> () {
// CHECK:   end_unpaired_access
// CHECK: } // end sil function '$S20preserve_exclusivity13f2___endNoOptyyBpF'

// DESERIALIZED-LABEL: sil [serialized] [_semantics "optimize.sil.preserve_exclusivity"] [canonical] @$S20preserve_exclusivity13f2___endNoOptyyBpF : $@convention(thin) (Builtin.RawPointer) -> () {
// DESERIALIZED:   end_unpaired_access
// DESERIALIZED: } // end sil function '$S20preserve_exclusivity13f2___endNoOptyyBpF'

@inlinable
@_semantics("optimize.sil.preserve_exclusivity")
public func f2___endNoOpt(_ address: Builtin.RawPointer) {
  marker2()
  Builtin.endUnpairedAccess(address)
}

// CHECK-LABEL: sil [_semantics "optimize.sil.preserve_exclusivity"] @$S20preserve_exclusivity13f3__readNoOptyyBp_BpmtF : $@convention(thin) (Builtin.RawPointer, @thin Builtin.RawPointer.Type) -> () {
// CHECK:   begin_unpaired_access
// CHECK: } // end sil function '$S20preserve_exclusivity13f3__readNoOptyyBp_BpmtF

// DESERIALIZED-LABEL: sil [serialized] [_semantics "optimize.sil.preserve_exclusivity"] [canonical] @$S20preserve_exclusivity13f3__readNoOptyyBp_BpmtF : $@convention(thin) (Builtin.RawPointer, @thin Builtin.RawPointer.Type) -> () {
// DESERIALIZED:   begin_unpaired_access
// DESERIALIZED: } // end sil function '$S20preserve_exclusivity13f3__readNoOptyyBp_BpmtF

@inlinable
@_semantics("optimize.sil.preserve_exclusivity")
public func f3__readNoOpt(_ address: Builtin.RawPointer, _ ty1: Builtin.RawPointer.Type) {
  marker3()
  Builtin.performInstantaneousReadAccess(address, ty1);
}

// DESERIALIZED-LABEL: sil [serialized] [canonical] @$S20preserve_exclusivity13f4__testNoOptyyBpF : $@convention(thin) (Builtin.RawPointer) -> () {
// DESERIALIZED: marker1
// DESERIALIZED: begin_unpaired_access
// DESERIALIZED: marker2
// DESERIALIZED: end_unpaired_access
// DESERIALIZED: marker3
// DESERIALIZED: begin_unpaired_access
// DESERIALIZED: return

@inlinable
public func f4__testNoOpt(_ k1: Builtin.RawPointer) {
  f1_beginNoOpt(k1, k1, Builtin.RawPointer.self)
  f2___endNoOpt(k1)
  f3__readNoOpt(k1, Builtin.RawPointer.self)
}

// CHECK-LABEL: sil @$S20preserve_exclusivity13f5_beginDoOptyyBp_BpxmtlF : $@convention(thin) <T1> (Builtin.RawPointer, Builtin.RawPointer, @thick T1.Type) -> () {
// CHECK-NOT: begin_unpaired_access
// CHECK: } // end sil function '$S20preserve_exclusivity13f5_beginDoOptyyBp_BpxmtlF'

// DESERIALIZED-LABEL: sil [serialized] [canonical] @$S20preserve_exclusivity13f5_beginDoOptyyBp_BpxmtlF : $@convention(thin) <T1> (Builtin.RawPointer, Builtin.RawPointer, @thick T1.Type) -> () {
// DESERIALIZED-NOT: begin_unpaired_access
// DESERIALIZED: } // end sil function '$S20preserve_exclusivity13f5_beginDoOptyyBp_BpxmtlF'

@inlinable
public func f5_beginDoOpt<T1>(_ address: Builtin.RawPointer, _ scratch: Builtin.RawPointer, _ ty1: T1.Type) {
  marker4()
  Builtin.beginUnpairedModifyAccess(address, scratch, ty1);
}

// CHECK-LABEL: sil @$S20preserve_exclusivity13f6___endDoOptyyBpF : $@convention(thin) (Builtin.RawPointer) -> () {
// CHECK-NOT: end_unpaired_access
// CHECK: } // end sil function '$S20preserve_exclusivity13f6___endDoOptyyBpF'

// DESERIALIZED-LABEL: sil [serialized] [canonical] @$S20preserve_exclusivity13f6___endDoOptyyBpF : $@convention(thin) (Builtin.RawPointer) -> () {
// DESERIALIZED-NOT: end_unpaired_access
// DESERIALIZED: } // end sil function '$S20preserve_exclusivity13f6___endDoOptyyBpF'

@inlinable
public func f6___endDoOpt(_ address: Builtin.RawPointer) {
  marker5()
  Builtin.endUnpairedAccess(address)
}

// CHECK-LABEL: sil @$S20preserve_exclusivity13f7__readDoOptyyBp_BpmtF : $@convention(thin) (Builtin.RawPointer, @thin Builtin.RawPointer.Type) -> () {
// CHECK-NOT: begin_unpaired_access
// CHECK: } // end sil function '$S20preserve_exclusivity13f7__readDoOptyyBp_BpmtF'

// DESERIALIZED-LABEL: sil [serialized] [canonical] @$S20preserve_exclusivity13f7__readDoOptyyBp_BpmtF : $@convention(thin) (Builtin.RawPointer, @thin Builtin.RawPointer.Type) -> () {
// DESERIALIZED-NOT: begin_unpaired_access
// DESERIALIZED: } // end sil function '$S20preserve_exclusivity13f7__readDoOptyyBp_BpmtF'

@inlinable
public func f7__readDoOpt(_ address: Builtin.RawPointer, _ ty1: Builtin.RawPointer.Type) {
  marker6()
  Builtin.performInstantaneousReadAccess(address, ty1);
}

// DESERIALIZED-LABEL: sil [serialized] [canonical] @$S20preserve_exclusivity13f8__testDoOptyyBpF : $@convention(thin) (Builtin.RawPointer) -> () {
// DESERIALIZED: marker4
// DESERIALIZED: marker5
// DESERIALIZED: marker6
// DESERIALIZED: return
@inlinable
public func f8__testDoOpt(_ k1: Builtin.RawPointer) {
  f5_beginDoOpt(k1, k1, Builtin.RawPointer.self)
  f6___endDoOpt(k1)
  f7__readDoOpt(k1, Builtin.RawPointer.self)
}
