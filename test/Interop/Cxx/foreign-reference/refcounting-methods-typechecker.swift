// RUN: %target-typecheck-verify-swift -Xcc -DINCORRECT -I %S%{fs-sep}Inputs -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}refcounting-methods.h -cxx-interoperability-mode=upcoming-swift -target %target-swift-5.8-abi-triple

import RefCountingMethods

let _ = StaticRetainRelease(123)
let _ = DerivedStaticRetainRelease(123, 456)
let _ = SharedAB(123)

let _ = AmbiguousReleaseMethods(321)
let _ = AmbiguousFreeReleaseAndRetainMethods(456)

func useMixedImmortalRetainRelease(_ x: MixedImmortalRetainRelease) {}
let _ = DerivedFromMixedImmortal(123)

func testDirectRetainReleaseCalls(
  box: RefCountedBox,
  derivedBox: DerivedRefCountedBox,
  derivedRelease: DerivedHasRelease,
  templated: TemplatedDerivedHasReleaseInt,
  crtp: CRTPDerived,
  virtualOps: VirtualRetainRelease,
  derivedVirtualOps: DerivedVirtualRetainRelease,
  pureVirtualOps: DerivedPureVirtualRetainRelease,
  ambiguousRelease: AmbiguousReleaseMethods,
  ambiguousFree: AmbiguousFreeReleaseAndRetainMethods
) {
  box.doRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'doRetain()'}}
  box.doRelease()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'doRelease()'}}
  unsafe box.doRetain()
  unsafe box.doRelease()

  let _ = box.doRetain
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'doRetain()'}}

  // Inherited: the derived type sees clones of the base's members.
  derivedBox.doRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'doRetain()'}}

  derivedRelease.doRetainInBase()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'doRetainInBase()'}}
  derivedRelease.doRelease()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'doRelease()'}}

  templated.doRetainInBase()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'doRetainInBase()'}}
  templated.doReleaseTemplated()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'doReleaseTemplated()'}}

  crtp.crtpRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'crtpRetain()'}}
  crtp.crtpRelease()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'crtpRelease()'}}

  virtualOps.doRetainVirtual()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'doRetainVirtual()'}}
  derivedVirtualOps.doRetainVirtual()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'doRetainVirtual()'}}

  pureVirtualOps.doRetainPure()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'doRetainPure()'}}

  ambiguousRelease.doRelease()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'doRelease()'}}
  ambiguousRelease.doRelease(1)

  retainAmbiguousFreeReleaseAndRetainMethods(ambiguousFree)
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe global function 'retainAmbiguousFreeReleaseAndRetainMethods'}}
  releaseAmbiguousFreeReleaseAndRetainMethods(ambiguousFree)
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe global function 'releaseAmbiguousFreeReleaseAndRetainMethods'}}

  unsafe retainAmbiguousFreeReleaseAndRetainMethods(ambiguousFree)
  unsafe releaseAmbiguousFreeReleaseAndRetainMethods(ambiguousFree)
}

func testOnlyViaFunction() {
  let _ = retainOnlyViaFunctionBox
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe global function 'retainOnlyViaFunctionBox'}}
  let _ = releaseOnlyViaFunctionBox
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe global function 'releaseOnlyViaFunctionBox'}}
}

func testDerivedWithOwnAnnotation(
  immortal: ImmortalDerivedFromShared,
  unsafeRef: UnsafeDerivedFromShared,
  ownOps: OwnOpsDerivedFromShared
) {
  immortal.baseRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'baseRetain()'}}

  unsafeRef.baseRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'baseRetain()'}}
  unsafe unsafeRef.baseRetain()

  ownOps.baseRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'baseRetain()'}}

  ownOps.ownRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'ownRetain()'}}
}

func testOverridesOfVirtualOperations(
  overrides: OverridesVirtualOps,
  middle: DoesNotOverrideVirtualOps,
  skipping: OverridesSkippingMiddle,
  nonPrimary: OverridesWithNonPrimaryFRTBase,
  inBothBases: OverridesInBothBases,
  throughVirtualBase: OverridesThroughVirtualBase,
  templated: TemplatedOverrideInt,
  fromNonFRTBase: OverridesOpsOfNonFRTBase,
  swiftNamed: OverridesSwiftNamedOps,
) {
  overrides.virtualRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'virtualRetain()'}}
  let _ = overrides.virtualRetain
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'virtualRetain()'}}
  unsafe overrides.virtualRetain()
  unsafe overrides.virtualRelease()

  middle.virtualRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'virtualRetain()'}}
  skipping.virtualRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'virtualRetain()'}}

  nonPrimary.virtualRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'virtualRetain()'}}

  inBothBases.virtualRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'virtualRetain()'}}

  throughVirtualBase.virtualRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'virtualRetain()'}}

  templated.virtualRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'virtualRetain()'}}

  // The operations come from a base that is not a foreign reference type.
  fromNonFRTBase.inheritedRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'inheritedRetain()'}}
  fromNonFRTBase.inheritedRelease()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'inheritedRelease()'}}

  swiftNamed.swiftRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'swiftRetain()'}}
}

func testOtherOverrides(
  overrides: OverridesVirtualOps,
  nonPrimary: OverridesWithNonPrimaryFRTBase,
  throughVirtualBase: OverridesThroughVirtualBase,
  reannotated: ReannotatedOverride
) {
  _ = overrides.unrelatedVirtual()
  _ = overrides.explicitlyUnsafeVirtual()
  _ = nonPrimary.unrelatedVirtual()
  _ = throughVirtualBase.unrelatedVirtual()
  _ = reannotated.unrelatedVirtual()
}

func testMoreOverrideShapes(
  diamond: DiamondBottom,
) {
  // Overrides through two paths to the same virtual base.
  diamond.virtualRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'virtualRetain()'}}
  diamond.virtualRelease()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'virtualRelease()'}}
}

func testReannotatedOverrides(
  reannotated: ReannotatedOverride,
  immortal: ImmortalReannotatedOverride
) {
  reannotated.virtualRetain()
  reannotated.virtualRelease()

  reannotated.ownRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'ownRetain()'}}

  immortal.virtualRetain()
  immortal.virtualRelease()
}

func testOverrideOfOnlyDerivedNamed(onlyDerived: OnlyDerivedNamed) {
  onlyDerived.onlyRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'onlyRetain()'}}
  onlyDerived.onlyRelease()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'onlyRelease()'}}
}

func testHidingNonVirtualOperations(
  base: NonVirtualOps,
  hides: HidesNonVirtualOps,
  hidesWithVirtual: HidesNonVirtualOpsWithVirtual,
  overridesHiding: OverridesVirtualHidingNonVirtualOps,
  overridesOther: OverridesOnlyOtherBase
) {
  base.plainRetain()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'plainRetain()'}}
  base.plainRelease()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'plainRelease()'}}

  hides.plainRetain()
  hides.plainRelease()
  hidesWithVirtual.plainRetain()
  hidesWithVirtual.plainRelease()
  overridesHiding.plainRetain()
  overridesHiding.plainRelease()
  overridesOther.plainRetain()

  overridesOther.plainRelease()
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'plainRelease()'}}
}
