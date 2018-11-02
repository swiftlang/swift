// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | %FileCheck --check-prefix=SILGEN %s
//
// Check that SILGen emits the correct SIL attributes for @_semantics("optimize.sil.preserve_exclusivity")/

// RUN: %target-swift-frontend -parse-stdlib -parse-stdlib -emit-sil -Onone %s | %FileCheck --check-prefix=CANONICAL %s
//
// Check that -Onone pipeline does not eliminate the attribute or access markers.

@_silgen_name("marker1")
func marker1() -> ()

@_silgen_name("marker2")
func marker2() -> ()

@_silgen_name("marker3")
func marker3() -> ()

@_silgen_name("marker4")
func marker4() -> ()

@_silgen_name("marker5")
func marker5() -> ()

@_silgen_name("marker6")
func marker6() -> ()

// SILGEN-LABEL: sil [_semantics "optimize.sil.preserve_exclusivity"] @$S20preserve_exclusivity10beginNoOptyyBp_BpxmtlF : $@convention(thin) <T1> (Builtin.RawPointer, Builtin.RawPointer, @thick T1.Type) -> () {
// SILGEN:   begin_unpaired_access
// SILGEN: } // end sil function '$S20preserve_exclusivity10beginNoOptyyBp_BpxmtlF

// CANONICAL-LABEL: sil [_semantics "optimize.sil.preserve_exclusivity"] @$S20preserve_exclusivity10beginNoOptyyBp_BpxmtlF : $@convention(thin) <T1> (Builtin.RawPointer, Builtin.RawPointer, @thick T1.Type) -> () {
// CANONICAL:   begin_unpaired_access
// CANONICAL: } // end sil function '$S20preserve_exclusivity10beginNoOptyyBp_BpxmtlF

@_semantics("optimize.sil.preserve_exclusivity")
public func beginNoOpt<T1>(_ address: Builtin.RawPointer, _ scratch: Builtin.RawPointer, _ ty1: T1.Type) {
  marker1()
  Builtin.beginUnpairedModifyAccess(address, scratch, ty1);
}

// SILGEN-LABEL: sil [_semantics "optimize.sil.preserve_exclusivity"] @$S20preserve_exclusivity8endNoOptyyBpF : $@convention(thin) (Builtin.RawPointer) -> () {
// SILGEN:   end_unpaired_access
// SILGEN: } // end sil function '$S20preserve_exclusivity8endNoOptyyBpF'

// CANONICAL-LABEL: sil [_semantics "optimize.sil.preserve_exclusivity"] @$S20preserve_exclusivity8endNoOptyyBpF : $@convention(thin) (Builtin.RawPointer) -> () {
// CANONICAL:   end_unpaired_access
// CANONICAL: } // end sil function '$S20preserve_exclusivity8endNoOptyyBpF'

@_semantics("optimize.sil.preserve_exclusivity")
public func endNoOpt(_ address: Builtin.RawPointer) {
  marker2()
  Builtin.endUnpairedAccess(address)
}

// SILGEN-LABEL: sil [_semantics "optimize.sil.preserve_exclusivity"] @$S20preserve_exclusivity9readNoOptyyBp_BpmtF : $@convention(thin) (Builtin.RawPointer, @thin Builtin.RawPointer.Type) -> () {
// SILGEN:   begin_unpaired_access
// SILGEN: } // end sil function '$S20preserve_exclusivity9readNoOptyyBp_BpmtF'

// CANONICAL-LABEL: sil [_semantics "optimize.sil.preserve_exclusivity"] @$S20preserve_exclusivity9readNoOptyyBp_BpmtF : $@convention(thin) (Builtin.RawPointer, @thin Builtin.RawPointer.Type) -> () {
// CANONICAL:   begin_unpaired_access
// CANONICAL: } // end sil function '$S20preserve_exclusivity9readNoOptyyBp_BpmtF'

@_semantics("optimize.sil.preserve_exclusivity")
public func readNoOpt(_ address: Builtin.RawPointer, _ ty1: Builtin.RawPointer.Type) {
  marker3()
  Builtin.performInstantaneousReadAccess(address, ty1);
}

public func testNoOpt(_ k1: Builtin.RawPointer) {
  beginNoOpt(k1, k1, Builtin.RawPointer.self)
  endNoOpt(k1)
  readNoOpt(k1, Builtin.RawPointer.self)
}

// SILGEN-LABEL: sil @$S20preserve_exclusivity8beginOptyyBp_BpxmtlF : $@convention(thin) <T1> (Builtin.RawPointer, Builtin.RawPointer, @thick T1.Type) -> () {
// SILGEN: begin_unpaired_access
// SILGEN: } // end sil function '$S20preserve_exclusivity8beginOptyyBp_BpxmtlF'

// CANONICAL-LABEL: sil @$S20preserve_exclusivity8beginOptyyBp_BpxmtlF : $@convention(thin) <T1> (Builtin.RawPointer, Builtin.RawPointer, @thick T1.Type) -> () {
// CANONICAL: begin_unpaired_access
// CANONICAL: } // end sil function '$S20preserve_exclusivity8beginOptyyBp_BpxmtlF'

public func beginOpt<T1>(_ address: Builtin.RawPointer, _ scratch: Builtin.RawPointer, _ ty1: T1.Type) {
  marker4()
  Builtin.beginUnpairedModifyAccess(address, scratch, ty1);
}

// SILGEN-LABEL: sil @$S20preserve_exclusivity6endOptyyBpF : $@convention(thin) (Builtin.RawPointer) -> () {
// SILGEN: end_unpaired_access
// SILGEN: } // end sil function '$S20preserve_exclusivity6endOptyyBpF'

// CANONICAL-LABEL: sil @$S20preserve_exclusivity6endOptyyBpF : $@convention(thin) (Builtin.RawPointer) -> () {
// CANONICAL: end_unpaired_access
// CANONICAL: } // end sil function '$S20preserve_exclusivity6endOptyyBpF'

public func endOpt(_ address: Builtin.RawPointer) {
  marker5()
  Builtin.endUnpairedAccess(address)
}

// SILGEN-LABEL: sil @$S20preserve_exclusivity7readOptyyBp_BpmtF : $@convention(thin) (Builtin.RawPointer, @thin Builtin.RawPointer.Type) -> () {
// SILGEN: begin_unpaired_access
// SILGEN: } // end sil function '$S20preserve_exclusivity7readOptyyBp_BpmtF'

// CANONICAL-LABEL: sil @$S20preserve_exclusivity7readOptyyBp_BpmtF : $@convention(thin) (Builtin.RawPointer, @thin Builtin.RawPointer.Type) -> () {
// CANONICAL: begin_unpaired_access
// CANONICAL: } // end sil function '$S20preserve_exclusivity7readOptyyBp_BpmtF'

public func readOpt(_ address: Builtin.RawPointer, _ ty1: Builtin.RawPointer.Type) {
  marker6()
  Builtin.performInstantaneousReadAccess(address, ty1);
}

public func testOpt(_ k1: Builtin.RawPointer) {
  beginOpt(k1, k1, Builtin.RawPointer.self)
  endOpt(k1)
  readOpt(k1, Builtin.RawPointer.self)
}
