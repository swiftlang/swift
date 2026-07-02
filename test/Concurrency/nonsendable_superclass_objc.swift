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

__attribute__((swift_attr("@MainActor")))
@interface ObjCResponder : NSObject
@end

__attribute__((swift_attr("@MainActor")))
@interface ObjCController : ObjCResponder
@end

//--- ObjCActorModule.m

#import "ObjCActorModule.h"

@implementation ObjCResponder
@end

@implementation ObjCController
@end

//--- main.swift

import ObjCActorModule

// A protocol conformance that refines Sendable should not trigger the
// non-Sendable superclass diagnostic when the superclass is already
// implicitly Sendable through the @MainActor chain.
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

// ObjC defines:
//   @MainActor ObjCResponder : NSObject
//   @MainActor ObjCController : ObjCResponder
//
// A Swift subclass of ObjCController inherits @MainActor isolation and
// should be implicitly Sendable with no diagnostic.

class MyController: ObjCController {} // no diagnostic expected

func requiresSendable<T: Sendable>(_: T) {}
func testObjCChainIsSendable(_ r: ObjCResponder, _ c: ObjCController, _ m: MyController) {
  requiresSendable(r)
  requiresSendable(c)
  requiresSendable(m)
}
