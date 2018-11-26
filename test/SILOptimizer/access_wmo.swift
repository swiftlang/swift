// RUN: %target-swift-frontend -parse-as-library -emit-sil -enforce-exclusivity=checked -O -primary-file %s %S/Inputs/access_wmo_def.swift | %FileCheck %s --check-prefix=PRIMARY
// RUN: %target-swift-frontend -parse-as-library -emit-sil -enforce-exclusivity=checked -O %s %S/Inputs/access_wmo_def.swift | %FileCheck %s --check-prefix=WMO

// ===---------------------------------------------------------------------===//
// testAccessGlobal: Access a global defined in another file.
//
// In -primary-file mode, all begin_access markers remain dynamic.
//
// With WMO, only the begin_access markers for the internal global should be
// promoted to static.
// ===---------------------------------------------------------------------===//

// readGlobal():
// PRIMARY-LABEL: sil @$S10access_wmo10readGlobalSiyF : $@convention(thin) () -> Int {
// function_ref internalGlobal.unsafeMutableAddressor
// PRIMARY: [[F1:%.*]] = function_ref @$S10access_wmo14internalGlobalSivau : $@convention(thin) () -> Builtin.RawPointer
// PRIMARY: [[G1:%.*]] = apply [[F1]]() : $@convention(thin) () -> Builtin.RawPointer
// PRIMARY: [[P1:%.*]] = pointer_to_address [[G1]] : $Builtin.RawPointer to [strict] $*Int
// PRIMARY: [[A1:%.*]] = begin_access [read] [dynamic] [no_nested_conflict] [[P1]] : $*Int
// PRIMARY: end_access [[A1]] : $*Int
// function_ref publicGlobal.unsafeMutableAddressor
// PRIMARY: [[F2:%.*]] = function_ref @$S10access_wmo12publicGlobalSivau : $@convention(thin) () -> Builtin.RawPointer
// PRIMARY: [[G2:%.*]] = apply [[F2]]() : $@convention(thin) () -> Builtin.RawPointer
// PRIMARY: [[P2:%.*]] = pointer_to_address [[G2]] : $Builtin.RawPointer to [strict] $*Int
// PRIMARY: [[A2:%.*]] = begin_access [read] [dynamic] [no_nested_conflict] [[P2]] : $*Int
// PRIMARY: end_access [[A2]] : $*Int
// PRIMARY-LABEL: } // end sil function '$S10access_wmo10readGlobalSiyF'
//
// WMO-LABEL: sil @$S10access_wmo10readGlobalSiyF : $@convention(thin) () -> Int {
// WMO: [[G1:%.*]] = global_addr @$S10access_wmo14internalGlobalSivp : $*Int
// WMO: [[A1:%.*]] = begin_access [read] [static] [no_nested_conflict] [[G1]] : $*Int
// WMO: end_access [[A1]] : $*Int
// WMO: [[G2:%.*]] = global_addr @$S10access_wmo12publicGlobalSivp : $*Int
// WMO: [[A2:%.*]] = begin_access [read] [dynamic] [no_nested_conflict] [[G2]] : $*Int
// WMO: end_access [[A2]] : $*Int
// WMO-LABEL: } // end sil function '$S10access_wmo10readGlobalSiyF'
public func readGlobal() -> Int {
  return internalGlobal + publicGlobal
}

@inline(never)
func setInt(_ i: inout Int, _ v: Int) {
  i = v
}

func inlinedSetInt(_ i: inout Int, _ v: Int) {
  i = v
}

// testAccessGlobal(v:)
// PRIMARY-LABEL: sil @$S10access_wmo16testAccessGlobal1vySi_tF : $@convention(thin) (Int) -> () {
// PRIMARY: bb0(%0 : $Int):
//
// function_ref internalGlobal.unsafeMutableAddressor
// PRIMARY: [[F1:%.*]] = function_ref @$S10access_wmo14internalGlobalSivau : $@convention(thin) () -> Builtin.RawPointer
// PRIMARY: [[G1:%.*]] = apply [[F1]]() : $@convention(thin) () -> Builtin.RawPointer
// PRIMARY: [[P1:%.*]] = pointer_to_address [[G1]] : $Builtin.RawPointer to [strict] $*Int
// PRIMARY: [[A1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[P1]] : $*Int
// function_ref setInt(_:_:)
// PRIMARY: [[F2:%.*]] = function_ref @$S10access_wmo6setIntyySiz_SitF : $@convention(thin) (@inout Int, Int) -> ()
// PRIMARY: apply [[F2]]([[A1]], %0) : $@convention(thin) (@inout Int, Int) -> ()
// PRIMARY: end_access [[A1]] : $*Int
//
// function_ref publicGlobal.unsafeMutableAddressor
// PRIMARY: [[F3:%.*]] = function_ref @$S10access_wmo12publicGlobalSivau : $@convention(thin) () -> Builtin.RawPointer
// PRIMARY: [[G2:%.*]] = apply [[F3]]() : $@convention(thin) () -> Builtin.RawPointer
// PRIMARY: [[P2:%.*]] = pointer_to_address [[G2]] : $Builtin.RawPointer to [strict] $*Int
// PRIMARY: [[A2:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[P2]] : $*Int
// PRIMARY: apply [[F2]]([[A2]], %0) : $@convention(thin) (@inout Int, Int) -> ()
// PRIMARY: end_access [[A2]] : $*Int
// PRIMARY-LABEL: } // end sil function '$S10access_wmo16testAccessGlobal1vySi_tF'
//
// WMO-LABEL: sil @$S10access_wmo16testAccessGlobal1vySi_tF : $@convention(thin) (Int) -> () {
// WMO: bb0(%0 : $Int):
// WMO: [[G1:%.*]] = global_addr @$S10access_wmo14internalGlobalSivp : $*Int
// WMO: [[A1:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[G1]] : $*Int
// function_ref setInt(_:_:)
// WMO: [[F:%.*]] = function_ref @$S10access_wmo6setIntyySiz_SitF : $@convention(thin) (@inout Int, Int) -> ()
// WMO: apply [[F]]([[A1]], %0) : $@convention(thin) (@inout Int, Int) -> ()
// WMO: end_access [[A1]] : $*Int
//
// WMO: [[G2:%.*]] = global_addr @$S10access_wmo12publicGlobalSivp : $*Int
// WMO: [[A2:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[G2]] : $*Int
// function_ref setInt(_:_:)
// WMO: apply [[F]]([[A2]], %0) : $@convention(thin) (@inout Int, Int) -> ()
// WMO: end_access [[A2]] : $*Int
// WMO-LABEL: } // end sil function '$S10access_wmo16testAccessGlobal1vySi_tF'
public func testAccessGlobal(v: Int) {
  setInt(&internalGlobal, v)
  setInt(&publicGlobal, v)
}

@inline(never)
func setKeyPath(_ c: C, _ kp: ReferenceWritableKeyPath<C, Int>, _ v: Int) {
  c[keyPath: kp] = v
}

// ===---------------------------------------------------------------------===//
// testAccessProp: Access a class property defined in another file.
//
// In -primary-file mode, all nonfinal access is behind
// getter, setter, or materializeForSet calls. The final access remains
// dynamic because the property is "visibleExternally". i.e. the compiler can't
// see how other files are using it.
// 
// With WMO...
//
// access to setterProp and finalProp are promoted to static.
// setterProp is a setter call, and finalProp is non-polymorphic.
//
// inlinedProp and internalProp could be also be promoted to static, but are
// accessed via begin_unpaired_access in materializeForSet. (When the class
// definition is in another file, the compiler unfortunately and unnecessarilly
// generates materializeForSet calls even in "wmo" mode). These unpaired access
// cannot (easily) be marked [no_nested_conflict]. Failing to mark the modify
// as [no_nested_access] in turn inhibits optimization of the property reads.
// Rather than teach the WMO to handle unpaired access, better solutions would
// be either:
//
// - The inlinedProp case could be handled with a separate pass that promotes
//   obvious unpaired access patterns to regular scoped access.
//
// - To handle internalProp SILGen could stop using materializeForSet for
//   stored properties:
//
// See <rdar://problem/41302183> [exclusivity] Optimize formal access from
// inlined materializeForSet.
//
// keyPathProp is promoted to static enforcement because a settable_property
// keypath can only be formally accessed within the getter and setter, which
// the compiler can already see.
//
// Perversely, finalKeyPathProp *cannot* be promoted to static enforcement
// because a storedProperty keypath is formed. The compiler does not currently
// attempt to track down all uses of a storedProperty keypath. The actual
// access occurs via a Builtin that takes a RawPointer. The formation of a
// stored_property keypath is always conservatively considered a possible
// nested access.
// ===---------------------------------------------------------------------===//
public func readProp(c: C) -> Int {
  return c.setterProp + c.finalProp + c.inlinedProp + c.internalProp
  + c.keyPathProp + c.finalKeyPathProp + c.publicProp
}
public func testAccessProp(c: C, v: Int) {
  c.setterProp = v
  setInt(&c.finalProp, v)
  inlinedSetInt(&c.inlinedProp, v)
  setInt(&c.internalProp, v)
  setKeyPath(c, \C.keyPathProp, v)
  setKeyPath(c, \C.finalKeyPathProp, v)
  setInt(&c.publicProp, v)
}

// PRIMARY-LABEL: sil @$S10access_wmo8readProp1cSiAA1CC_tF : $@convention(thin) (@guaranteed C) -> Int {
// PRIMARY-NOT: begin_{{.*}}access
// PRIMARY: [[E1:%.*]] = ref_element_addr %0 : $C, #C.finalProp
// PRIMARY: [[A1:%.*]] = begin_access [read] [dynamic] [no_nested_conflict] [[E1]] : $*Int
// PRIMARY: end_access [[A1]] : $*Int
// PRIMARY-NOT: begin_{{.*}}access
// PRIMARY: [[E2:%.*]] = ref_element_addr %0 : $C, #C.finalKeyPathProp
// PRIMARY: [[A2:%.*]] = begin_access [read] [dynamic] [no_nested_conflict] [[E2]] : $*Int
// PRIMARY: end_access [[A2]] : $*Int
// PRIMARY-NOT: begin_{{.*}}access
// PRIMARY-LABEL:  } // end sil function '$S10access_wmo8readProp1cSiAA1CC_tF'
//
// WMO-LABEL: sil @$S10access_wmo8readProp1cSiAA1CC_tF : $@convention(thin) (@guaranteed C) -> Int {
// WMO: [[E1:%.*]] = ref_element_addr %0 : $C, #C.setterProp
// WMO: [[A1:%.*]] = begin_access [read] [static] [no_nested_conflict] [[E1]] : $*Int
// WMO: end_access [[A1]] : $*Int
//
// WMO: [[E2:%.*]] = ref_element_addr %0 : $C, #C.finalProp
// WMO: [[A2:%.*]] = begin_access [read] [static] [no_nested_conflict] [[E2]] : $*Int
// WMO: end_access [[A2]] : $*Int
//
// WMO: [[E3:%.*]] = ref_element_addr %0 : $C, #C.inlinedProp
// WMO: [[A3:%.*]] = begin_access [read] [static] [no_nested_conflict] [[E3]] : $*Int
// WMO: end_access [[A3]] : $*Int
//
// WMO: [[E4:%.*]] = ref_element_addr %0 : $C, #C.internalProp
// WMO: [[A4:%.*]] = begin_access [read] [static] [no_nested_conflict] [[E4]] : $*Int
// WMO: end_access [[A4]] : $*Int
//
// WMO: [[E5:%.*]] = ref_element_addr %0 : $C, #C.keyPathProp
// WMO: [[A5:%.*]] = begin_access [read] [static] [no_nested_conflict] [[E5]] : $*Int
// WMO: end_access [[A5]] : $*Int
//
// WMO: [[E6:%.*]] = ref_element_addr %0 : $C, #C.finalKeyPathProp
// WMO: [[A6:%.*]] = begin_access [read] [dynamic] [no_nested_conflict] [[E6]] : $*Int
// WMO: end_access [[A6]] : $*Int
//
// WMO: [[E7:%.*]] = ref_element_addr %0 : $C, #C.publicProp
// WMO: [[A7:%.*]] = begin_access [read] [dynamic] [no_nested_conflict] [[E7]] : $*Int
// WMO: end_access [[A7]] : $*Int
// WMO-LABEL: } // end sil function '$S10access_wmo8readProp1cSiAA1CC_tF'

// PRIMARY-LABEL: sil @$S10access_wmo14testAccessProp1c1vyAA1CC_SitF : $@convention(thin) (@guaranteed C, Int) -> () {
// PRIMARY-NOT: begin_{{.*}}access
// PRIMARY: [[E1:%.*]] = ref_element_addr %0 : $C, #C.finalProp
// PRIMARY: [[A1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[E1]] : $*Int
// function_ref setInt(_:_:)
// PRIMARY: [[F1:%.*]] = function_ref @$S10access_wmo6setIntyySiz_SitF : $@convention(thin) (@inout Int, Int) -> ()
// PRIMARY: apply [[F1]]([[A1]], %1) : $@convention(thin) (@inout Int, Int) -> ()
// PRIMARY: end_access [[A1]] : $*Int
// PRIMARY-NOT: begin_{{.*}}access
// PRIMARY-LABEL: } // end sil function '$S10access_wmo14testAccessProp1c1vyAA1CC_SitF'

// WMO-LABEL: sil @$S10access_wmo14testAccessProp1c1vyAA1CC_SitF : $@convention(thin) (@guaranteed C, Int) -> () {
// WMO: [[E1:%.*]] = ref_element_addr %0 : $C, #C.setterProp
// WMO: [[A1:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[E1]] : $*Int
// WMO: end_access [[A1]] : $*Int
//
// WMO: [[E2:%.*]] = ref_element_addr %0 : $C, #C.finalProp
// WMO: [[A2:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[E2]] : $*Int
// function_ref setInt(_:_:)
// WMO: [[F1:%.*]] = function_ref @$S10access_wmo6setIntyySiz_SitF : $@convention(thin) (@inout Int, Int) -> ()
// WMO: apply [[F1]]([[A2]], %1) : $@convention(thin) (@inout Int, Int) -> ()
// WMO: end_access [[A2]] : $*Int
//
// WMO: [[E3:%.*]] = ref_element_addr %0 : $C, #C.inlinedProp
// WMO: [[A3:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[E3]] : $*Int
// WMO: end_access [[A3]] : $*Int
//
// WMO: [[E4:%.*]] = ref_element_addr %0 : $C, #C.internalProp
// WMO: [[A4:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[E4]] : $*Int
// WMO: apply [[F1]]([[A4]], %1) : $@convention(thin) (@inout Int, Int) -> ()
// WMO: end_access [[A4]]
//
// WMO: [[KP:%.*]] = keypath $ReferenceWritableKeyPath<C, Int>, (root $C; settable_property $Int,  id #C.keyPathProp!getter.1 : (C) -> () -> Int, getter @$S10access_wmo1CC11keyPathPropSivpACTK : $@convention(thin) (@in_guaranteed C) -> @out Int, setter @$S10access_wmo1CC11keyPathPropSivpACTk : $@convention(thin) (@in_guaranteed Int, @in_guaranteed C) -> ())
// function_ref setKeyPath(_:_:_:)
// WMO: [[F2:%.*]] = function_ref @$S10access_wmo10setKeyPathyyAA1CC_s017ReferenceWritabledE0CyADSiGSitF : $@convention(thin) (@guaranteed C, @guaranteed ReferenceWritableKeyPath<C, Int>, Int) -> ()
// WMO: apply [[F2]](%0, [[KP]], %1) : $@convention(thin) (@guaranteed C, @guaranteed ReferenceWritableKeyPath<C, Int>, Int) -> ()
//
// WMO: [[FKP:%.*]] = keypath $ReferenceWritableKeyPath<C, Int>, (root $C; stored_property #C.finalKeyPathProp : $Int)
// WMO: apply [[F2]](%0, [[FKP]], %1) : $@convention(thin) (@guaranteed C, @guaranteed ReferenceWritableKeyPath<C, Int>, Int) -> ()
//
// WMO: [[E4:%.*]] = ref_element_addr %0 : $C, #C.publicProp
// WMO: [[A4:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[E4]] : $*Int
// WMO: apply [[F1]]([[A4]], %1) : $@convention(thin) (@inout Int, Int) -> ()
// WMO: end_access [[A4]]
// WMO-LABEL: } // end sil function '$S10access_wmo14testAccessProp1c1vyAA1CC_SitF'
