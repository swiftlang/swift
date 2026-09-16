// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: %empty-directory(%t/sdk/ObjCActorModule)
// RUN: split-file %s %t/src

// Build ObjC module
// RUN: %target-clang -fmodules -dynamiclib %t/src/ObjCActorModule.m -I %t/src -o %t/sdk/ObjCActorModule/libObjCActorModule.dylib -lobjc -framework Foundation
// RUN: cp %t/src/ObjCActorModule.modulemap %t/sdk/ObjCActorModule/module.modulemap
// RUN: cp %t/src/ObjCActorModule.h %t/sdk/ObjCActorModule/ObjCActorModule.h

// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -strict-concurrency=complete %t/src/main.swift -emit-sil -o /dev/null -verify -I %t/sdk/ObjCActorModule
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %t/src/main.swift -emit-sil -o /dev/null -verify -verify-additional-prefix swift6- -swift-version 6 -I %t/sdk/ObjCActorModule

// REQUIRES: concurrency
// REQUIRES: objc_interop

//--- ObjCActorModule.modulemap

module ObjCActorModule {
  header "ObjCActorModule.h"
  export *
}

//--- ObjCActorModule.h

#import <Foundation/Foundation.h>

// Models the UIKit chain: NSObject -> @MainActor UIResponder -> @MainActor UIViewController/UIView
// The entire chain is @MainActor-isolated with a Sendable root (NSObject).

__attribute__((swift_attr("@MainActor")))
@interface ObjCResponder : NSObject
@end

__attribute__((swift_attr("@MainActor")))
@interface ObjCController : ObjCResponder
@end

__attribute__((swift_attr("@MainActor")))
@interface ObjCView : ObjCResponder
@end

//--- ObjCActorModule.m

#import "ObjCActorModule.h"

@implementation ObjCResponder
@end

@implementation ObjCController
@end

@implementation ObjCView
@end

//--- main.swift

import ObjCActorModule

// ============================================================================
// Implied Sendable through protocol refinement
// ============================================================================
//
// When Sendable comes through a protocol (e.g. MyDelegate: Sendable),
// the conformance table builds it as Implied -> NormalProtocolConformance
// because the superclass's implicit Sendable from @MainActor wasn't in
// the table yet when the Inherited stage ran.
//
// The protocol conformance must be checked BEFORE any code that would
// trigger eager Sendable derivation on ObjCController (e.g. a
// requiresSendable() call), because early derivation masks the bug by
// materializing an InheritedProtocolConformance.

protocol MyDelegate: Sendable {
  func didDoThing()
}

@MainActor
final class MyDelegateController: ObjCController {}

extension MyDelegateController: @preconcurrency MyDelegate {
  func didDoThing() {}
}

@MainActor
protocol MyIsolatedDelegate: Sendable {
  func didDoThing()
}

@MainActor
final class MyIsolatedDelegateController: ObjCController, MyIsolatedDelegate {
  func didDoThing() {}
}

// ============================================================================
// Explicit Sendable on @MainActor classes with @MainActor ObjC superclasses
// ============================================================================
//
// Real-world pattern: @MainActor final classes that inherit from UIView or
// UIViewController (modeled here as ObjCView / ObjCController) and explicitly
// declare Sendable. The superclass chain is @MainActor all the way down to
// NSObject, so the superclass IS Sendable. No diagnostic should fire.

@MainActor
final class ExplicitSendableView: ObjCView, Sendable {}

@MainActor
final class ExplicitSendableController: ObjCController, Sendable {}

// Public @MainActor view subclass with explicit Sendable, matching the
// pattern of public UI component views.
@MainActor
public final class PublicExplicitSendableView: ObjCView, Sendable {}

// ============================================================================
// Non-final @MainActor classes with explicit Sendable
// ============================================================================
//
// A non-final @MainActor class with a Sendable superclass chain is
// still valid: subclasses inherit @MainActor isolation, so they can't
// add unsynchronized state. The non-final restriction only applies to
// non-isolated classes.

@MainActor
class NonFinalExplicitSendableController: ObjCController, Sendable {}

// ============================================================================
// Implicit Sendable (no explicit annotation)
// ============================================================================
//
// ObjC defines:
//   @MainActor ObjCResponder : NSObject
//   @MainActor ObjCController : ObjCResponder
//   @MainActor ObjCView : ObjCResponder
//
// Swift subclasses inherit @MainActor isolation and should be implicitly
// Sendable with no diagnostic.

class MyController: ObjCController {} // no diagnostic expected
class MyView: ObjCView {} // no diagnostic expected

// ============================================================================
// Verify all of the above are usable as Sendable
// ============================================================================

func requiresSendable<T: Sendable>(_: T) {}
func testObjCChainIsSendable(
  _ r: ObjCResponder,
  _ c: ObjCController,
  _ v: ObjCView,
  _ mc: MyController,
  _ mv: MyView,
  _ dc: MyDelegateController,
  _ ic: MyIsolatedDelegateController,
  _ ec: ExplicitSendableController,
  _ ev: ExplicitSendableView,
  _ pv: PublicExplicitSendableView,
  _ nf: NonFinalExplicitSendableController
) {
  requiresSendable(r)
  requiresSendable(c)
  requiresSendable(v)
  requiresSendable(mc)
  requiresSendable(mv)
  requiresSendable(dc)
  requiresSendable(ic)
  requiresSendable(ec)
  requiresSendable(ev)
  requiresSendable(pv)
  requiresSendable(nf)
}
