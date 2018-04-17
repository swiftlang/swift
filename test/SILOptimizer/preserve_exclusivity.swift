// RUN: %target-swift-frontend -parse-stdlib -Xllvm -sil-disable-pass=FunctionSignatureOpts -Xllvm -sil-disable-pass=GenericSpecializer -parse-stdlib -emit-sil -O %s | %FileCheck %s

@_silgen_name("marker1")
func marker1() -> ()

@_silgen_name("marker2")
func marker2() -> ()

@_silgen_name("marker3")
func marker3() -> ()

@_silgen_name("marker4")
func marker4() -> ()

// CHECK: sil [noinline] [_semantics "optimize.sil.preserve_exclusivity"] @$S20preserve_exclusivity10beginNoOptyyBp_BpxmtlF : $@convention(thin) <T1> (Builtin.RawPointer, Builtin.RawPointer, @thick T1.Type) -> () {
// CHECK:   begin_unpaired_access
// CHECK: } // end sil function '$S20preserve_exclusivity10beginNoOptyyBp_BpxmtlF'

@inline(never)
@_semantics("optimize.sil.preserve_exclusivity")
public func beginNoOpt<T1>(_ address: Builtin.RawPointer, _ scratch: Builtin.RawPointer, _ ty1: T1.Type) {
  marker1()
  Builtin.beginUnpairedModifyAccess(address, scratch, ty1);
}

// CHECK: sil [noinline] [_semantics "optimize.sil.preserve_exclusivity"] @$S20preserve_exclusivity8endNoOptyyBpF : $@convention(thin) (Builtin.RawPointer) -> () {
// CHECK:   end_unpaired_access
// CHECK: } // end sil function '$S20preserve_exclusivity8endNoOptyyBpF'

@inline(never)
@_semantics("optimize.sil.preserve_exclusivity")
public func endNoOpt(_ address: Builtin.RawPointer) {
  marker2()
  Builtin.endUnpairedAccess(address)
}

class Klass {}

public func testNoOpt(_ k1: Builtin.RawPointer) {
  beginNoOpt(k1, k1, Builtin.RawPointer.self)
  endNoOpt(k1)
}

// CHECK: sil [noinline] @$S20preserve_exclusivity8beginOptyyBp_BpxmtlF : $@convention(thin) <T1> (Builtin.RawPointer, Builtin.RawPointer, @thick T1.Type) -> () {
// CHECK-NOT: begin_unpaired_access
// CHECK: } // end sil function '$S20preserve_exclusivity8beginOptyyBp_BpxmtlF'

@inline(never)
public func beginOpt<T1>(_ address: Builtin.RawPointer, _ scratch: Builtin.RawPointer, _ ty1: T1.Type) {
  marker3()
  Builtin.beginUnpairedModifyAccess(address, scratch, ty1);
}

// CHECK: sil [noinline] @$S20preserve_exclusivity6endOptyyBpF : $@convention(thin) (Builtin.RawPointer) -> () {
// CHECK-NOT: end_unpaired_access
// CHECK: } // end sil function '$S20preserve_exclusivity6endOptyyBpF'

@inline(never)
public func endOpt(_ address: Builtin.RawPointer) {
  marker4()
  Builtin.endUnpairedAccess(address)
}

public func testOpt(_ k1: Builtin.RawPointer) {
  beginOpt(k1, k1, Builtin.RawPointer.self)
  endOpt(k1)
}
