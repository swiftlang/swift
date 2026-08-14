// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -strict-memory-safety
//
// An incomplete reference type has no layout to ask Clang for, so check that
// lowering one does not crash either.
// RUN: %target-swift-frontend -emit-ir -o /dev/null %t%{fs-sep}test.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking

// A foreign reference type may be declared without a definition; it is still
// imported as a Swift class, and is still escapable and safe. Importing one used
// to crash, because member loading and conformance synthesis assumed a
// definition.

//--- Inputs/module.modulemap
module Test {
    header "incomplete.h"
    requires cplusplus
}

//--- Inputs/incomplete.h
#include "swift/bridging"

void retainFwd(struct FwdShared *);
void releaseFwd(struct FwdShared *);

// Declared, never defined.
struct SWIFT_IMMORTAL_REFERENCE FwdImmortal;
struct SWIFT_SHARED_REFERENCE(retainFwd, releaseFwd) FwdShared;

// A struct holding pointers to them. The pointers are imported as class
// references, so nothing here is unsafe.
struct HoldsIncompleteFRTs {
  FwdImmortal *immortal;
  FwdShared *shared;
};

FwdImmortal *makeImmortal();
FwdShared *makeShared() SWIFT_RETURNS_UNRETAINED;
void takeImmortal(FwdImmortal *);

// A non-escapable view over an incomplete reference type is a direct view: the
// pointee is managed by Swift, so nothing here can dangle.
struct SWIFT_NONESCAPABLE ViewOfIncomplete {
  FwdShared *shared;
};
ViewOfIncomplete makeView();

// Same shape without the reference annotation: not imported as a class, so this
// one stays unsafe. Keeps the test honest about -strict-memory-safety being on.
struct PlainFwd;
struct HoldsPlainFwd {
  PlainFwd *plain;
};
PlainFwd *makePlain();

// swift_attr only propagates forward, so 'retainLate' declares the type before
// the annotation is seen. Which declaration gets imported follows name order, so
// keep the reference type sorting after its holder.
void retainLate(struct ZLateAnnotated *);
void releaseLate(struct ZLateAnnotated *);
struct SWIFT_SHARED_REFERENCE(retainLate, releaseLate) ZLateAnnotated;
struct AHoldsLateAnnotated {
  ZLateAnnotated *late;
};

// SWIFT_NAME injects members via an extension, so they survive skipping member
// loading for incomplete records.
int getWeight(FwdImmortal *self) __attribute__((swift_name("getter:FwdImmortal.weight(self:)")));
void doThing(FwdImmortal *self) __attribute__((swift_name("FwdImmortal.doThing(self:)")));

// An unsafe reference type stays unsafe, whichever declaration is reached first.
void retainUnsafe(struct FwdUnsafe *);
void releaseUnsafe(struct FwdUnsafe *);
struct SWIFT_SHARED_REFERENCE(retainUnsafe, releaseUnsafe) SWIFT_UNSAFE FwdUnsafe;
struct HoldsFwdUnsafe {
  FwdUnsafe *p;
};

//--- test.swift
import Test

// The incomplete foreign reference types are imported as classes.
func useImmortal(_ x: FwdImmortal) { _ = x }
func useShared(_ x: FwdShared) { _ = x }
// 'public' so that IRGen emits metadata for it, which needs a layout.
public func useOptional(_ x: FwdImmortal?) { _ = x }

// Returning and passing them requires no 'unsafe'.
func roundTrip() {
  let x = makeImmortal()
  takeImmortal(x)
  let y = makeShared()
  _ = y
}

// A struct holding them is escapable and safe.
func useHolder(_ x: HoldsIncompleteFRTs) {
  _ = x
  _ = x.immortal
  _ = x.shared
}

// A non-escapable view over one is a direct view, so it is safe too.
func useView(_ x: ViewOfIncomplete) {
  _ = x
}

// A late-seen annotation still imports the field as a class reference.
func useLateAnnotated(_ x: AHoldsLateAnnotated) {
  _ = x
  _ = x.late
}

// SWIFT_NAME-injected members are still available.
func useInjectedMembers(_ x: FwdImmortal) {
  _ = x.weight
  x.doThing()
}

// These two differ only in which declaration is reached first; both must warn.
func useUnsafeFirst(_ x: FwdUnsafe) {
  _ = x // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'x' involves unsafe type 'FwdUnsafe'}}
}

func useUnsafeSecond(_ x: HoldsFwdUnsafe) {
  _ = x.p // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to property 'p' involves unsafe type 'FwdUnsafe'}}
}

// Without the annotation the pointer is imported as an opaque pointer, which
// confirms -strict-memory-safety is in effect above.
func usePlain(_ x: HoldsPlainFwd) {
  _ = x.plain // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to property 'plain' involves unsafe type 'OpaquePointer'}}
  // expected-note@-2 {{reference to parameter 'x' involves unsafe type 'HoldsPlainFwd'}}
}

func makePlainIsUnsafe() {
  let p = makePlain() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to global function 'makePlain()' involves unsafe type 'OpaquePointer'}}
  _ = p // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to let 'p' involves unsafe type 'OpaquePointer'}}
}
