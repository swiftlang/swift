// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/other_global_actor_inference.swiftmodule -module-name other_global_actor_inference -strict-concurrency=complete %S/Inputs/other_global_actor_inference.swift -enable-experimental-feature GlobalActorIsolatedTypesUsability
// RUN: %target-swift-frontend -I %t -disable-availability-checking %s -emit-sil -o /dev/null -verify -verify-additional-prefix minimal-targeted- -enable-experimental-feature GlobalActorIsolatedTypesUsability
// RUN: %target-swift-frontend -I %t -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted -verify-additional-prefix minimal-targeted- -enable-experimental-feature GlobalActorIsolatedTypesUsability
// RUN: %target-swift-frontend -I %t -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -verify-additional-prefix complete-tns- -enable-experimental-feature GlobalActorIsolatedTypesUsability
// RUN: %target-swift-frontend -I %t -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation -verify-additional-prefix complete-tns- -enable-experimental-feature GlobalActorIsolatedTypesUsability

// REQUIRES: concurrency
// REQUIRES: asserts

actor Container {
  var num: Int = 0 // expected-note{{mutation of this property is only permitted within the actor}}
  func test() async {

    // no warnings:
    await withTaskCancellationHandler {
      num += 1
    } onCancel: {
      // nothing
    }
  }

  func errors() async {
    await withTaskCancellationHandler {
      num += 1 // no error, this runs synchronously on caller context
    } onCancel: {
      // this should error because cancellation is invoked concurrently
      num += 10 // expected-error{{actor-isolated property 'num' can not be mutated from a Sendable closure}}
    }
  }
}