// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules %s -verify -strict-concurrency=complete -parse-as-library

// REQUIRES: objc_interop
// REQUIRES: concurrency
// REQUIRES: asserts

import Foundation
import ObjCConcurrency

// expected-note@+1 2{{add '@MainActor' to make global function 'unsatisfiedPreconcurrencyIsolation(view:)' part of global actor 'MainActor'}}
func unsatisfiedPreconcurrencyIsolation(view: MyView) {
  // expected-warning@+1 {{call to main actor-isolated instance method 'display()' in a synchronous nonisolated context}}
  view.display()

  // expected-warning@+1 {{main actor-isolated property 'isVisible' can not be referenced from a non-isolated context}}
  _ = view.isVisible
}
