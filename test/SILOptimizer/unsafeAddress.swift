// RUN: %target-swift-frontend -module-name Swift -emit-sil %s | %FileCheck %s

@_silgen_name("makeUpAPointer")
func makeUpAPointer<T: ~Copyable>() -> UnsafePointer<T>
@_silgen_name("makeUpAMutablePointer")
func makeUpAPointer<T: ~Copyable>() -> UnsafeMutablePointer<T>
@_silgen_name("makeUpAnInt")
func makeUpAnInt() -> Int

class X {}

struct NC: ~Copyable {
  var x: Any = X()
  deinit {}
}

struct S {
  var s: String

  var data: NC {
    unsafeAddress { return makeUpAPointer() }
  }

  var mutableData: NC {
    unsafeAddress { return makeUpAPointer() }
    unsafeMutableAddress { return makeUpAPointer() }
  }
}

struct SNC: ~Copyable {
  var data: NC {
    unsafeAddress { return makeUpAPointer() }
  }

  var mutableData: NC {
    unsafeAddress { return makeUpAPointer() }
    unsafeMutableAddress { return makeUpAPointer() }
  }
}

class C {
  final var data: NC {
    unsafeAddress { return makeUpAPointer() }
  }

  final var mutableData: NC {
    unsafeAddress { return makeUpAPointer() }
    unsafeMutableAddress { return makeUpAPointer() }
  }
}

func borrow(_ nc: borrowing NC) {}
func mod(_ nc: inout NC) {}

// CHECK-LABEL: sil hidden @$s4main11testCBorrow1cyAA1CC_tF : $@convention(thin) (@guaranteed C) -> () {
// CHECK: [[ADR:%.*]] = pointer_to_address %4 to [strict] $*NC
// CHECK: [[MD:%.*]] = mark_dependence [nonescaping] [[ADR]] on %0
// CHECK: begin_access [read] [unsafe] [[MD]]
// CHECK: apply
// CHECK: end_access
// CHECK-LABEL: } // end sil function '$s4main11testCBorrow1cyAA1CC_tF'
func testCBorrow(c: C) {
  borrow(c.data)
}

// CHECK-LABEL: sil hidden @$s4main8testCMod1cyAA1CC_tF : $@convention(thin) (@guaranteed C) -> () {
// CHECK: [[ADR:%.*]] = pointer_to_address %{{.*}} to [strict] $*NC
// CHECK: [[MD:%.*]] = mark_dependence [nonescaping] [[ADR]] on %0
// CHECK: begin_access [modify] [unsafe] [[MD]]
// CHECK: apply
// CHECK: end_access
// CHECK-LABEL: } // end sil function '$s4main8testCMod1cyAA1CC_tF'
func testCMod(c: C) {
  mod(&c.mutableData)
}

// CHECK-LABEL: sil hidden @$s4main11testSBorrow1syAA1SV_tF : $@convention(thin) (@guaranteed S) -> () {
// CHECK: [[ADR:%.*]] = pointer_to_address %{{.*}} to [strict] $*NC
// CHECK: [[MD:%.*]] = mark_dependence [nonescaping] [[ADR]] on %0
// CHECK: begin_access [read] [unsafe] [[MD]]
// CHECK: apply
// CHECK: end_access
// CHECK-LABEL: } // end sil function '$s4main11testSBorrow1syAA1SV_tF'
func testSBorrow(s: S) {
  borrow(s.data)
}

// CHECK-LABEL: sil hidden @$s4main8testSMod1syAA1SVz_tF : $@convention(thin) (@inout S) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [static] %0
// CHECK: [[ADR:%.*]] = pointer_to_address %{{.*}} to [strict] $*NC
// CHECK: [[MD:%.*]] = mark_dependence [nonescaping] [[ADR]] on [[ACCESS]]
// CHECK: begin_access [modify] [unsafe] [[MD]]
// CHECK: apply
// CHECK: end_access
// CHECK-LABEL: } // end sil function '$s4main8testSMod1syAA1SVz_tF'
func testSMod(s: inout S) {
  mod(&s.mutableData)
}

// Accessing s.data causes an escaping dependence because we don't extend the local access scope of 's'.
//
// CHECK-LABEL: sil hidden @$s4main16testSInoutBorrow5mut_syAA1SVz_tF : $@convention(thin) (@inout S) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [read] [static] %0
// CHECK: [[LD:%.*]] = load [[ACCESS]]
// CHECK: [[ADR:%.*]] = pointer_to_address %{{.*}} to [strict] $*NC
// CHECK: [[MD:%.*]] = mark_dependence [nonescaping] [[ADR]] on [[LD]]
// CHECK: begin_access [read] [unsafe] [[MD]]
// CHECK: apply
// CHECK: end_access
// CHECK-LABEL: } // end sil function '$s4main16testSInoutBorrow5mut_syAA1SVz_tF'
func testSInoutBorrow(mut_s s: inout S) {
  borrow(s.data)
}

// CHECK-LABEL: sil hidden @$s4main19testSInoutMutBorrow5mut_syAA1SVz_tF : $@convention(thin) (@inout S) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [read] [static] %0
// CHECK: [[LD:%.*]] = load [[ACCESS]]
// CHECK: [[ADR:%.*]] = pointer_to_address %{{.*}} to [strict] $*NC
// CHECK: [[MD:%.*]] = mark_dependence [nonescaping] [[ADR]] on [[ACCESS]]
// CHECK: begin_access [read] [unsafe] [[MD]]
// CHECK: apply
// CHECK: end_access
// CHECK-LABEL: } // end sil function '$s4main19testSInoutMutBorrow5mut_syAA1SVz_tF'
func testSInoutMutBorrow(mut_s s: inout S) {
  borrow(s.mutableData)
}

// CHECK-LABEL: sil hidden @$s4main13testSInoutMod5mut_syAA1SVz_tF : $@convention(thin) (@inout S) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [static] %0
// CHECK: [[ADR:%.*]] = pointer_to_address %{{.*}} to [strict] $*NC
// CHECK: [[MD:%.*]] = mark_dependence [nonescaping] [[ADR]] on [[ACCESS]]
// CHECK: begin_access [modify] [unsafe] [[MD]]
// CHECK: apply
// CHECK: end_access
// CHECK-LABEL: } // end sil function '$s4main13testSInoutMod5mut_syAA1SVz_tF'
func testSInoutMod(mut_s s: inout S) {
  mod(&s.mutableData)
}

// CHECK-LABEL: sil hidden @$s4main13testSNCBorrow3sncyAA3SNCV_tF : $@convention(thin) (@guaranteed SNC) -> () {
// CHECK: [[ADR:%.*]] = pointer_to_address %{{.*}} to [strict] $*NC
// CHECK: [[MD:%.*]] = mark_dependence [nonescaping] [[ADR]] on %0
// CHECK: begin_access [read] [unsafe] [[MD]]
// CHECK: apply
// CHECK: end_access
// CHECK-LABEL: } // end sil function '$s4main13testSNCBorrow3sncyAA3SNCV_tF'
func testSNCBorrow(snc: borrowing SNC) {
  borrow(snc.data)
}

// CHECK-LABEL: sil hidden @$s4main16testSNCMutBorrow3sncyAA3SNCV_tF : $@convention(thin) (@guaranteed SNC) -> () {
// CHECK: [[ADR:%.*]] = pointer_to_address %{{.*}} to [strict] $*NC
// CHECK: [[MD:%.*]] = mark_dependence [nonescaping] [[ADR]] on %0
// CHECK: begin_access [read] [unsafe] [[MD]]
// CHECK: apply
// CHECK: end_access
// CHECK-LABEL: } // end sil function '$s4main16testSNCMutBorrow3sncyAA3SNCV_tF'
func testSNCMutBorrow(snc: borrowing SNC) {
  borrow(snc.mutableData)
}

// CHECK-LABEL: sil hidden @$s4main10testSNCMod7mut_sncyAA3SNCVz_tF : $@convention(thin) (@inout SNC) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [static] %0
// CHECK: [[ADR:%.*]] = pointer_to_address %{{.*}} to [strict] $*NC
// CHECK: [[MD:%.*]] = mark_dependence [nonescaping] [[ADR]] on [[ACCESS]]
// CHECK: begin_access [modify] [unsafe] [[MD]]
// CHECK: apply
// CHECK: end_access
// CHECK-LABEL: } // end sil function '$s4main10testSNCMod7mut_sncyAA3SNCVz_tF'
func testSNCMod(mut_snc snc: inout SNC) {
  mod(&snc.mutableData)
}
