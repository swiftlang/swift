// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Non-primary's default is respected.
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -typecheck -swift-version 6 -disable-availability-checking -verify -primary-file %t/dir_np_respected/plain.swift %t/dir_np_respected/withUsing.swift

// Primary's default doesn't leak to non-primary.
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -typecheck -swift-version 6 -disable-availability-checking -verify -primary-file %t/dir_p_no_bleed/withUsing.swift %t/dir_p_no_bleed/plain.swift

// Non-primary's default doesn't leak to primary.
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -typecheck -swift-version 6 -disable-availability-checking -verify -primary-file %t/dir_np_no_bleed/plain.swift %t/dir_np_no_bleed/withUsing.swift

// Both files have defaults.
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -typecheck -swift-version 6 -disable-availability-checking -verify -primary-file %t/dir_both/primary.swift %t/dir_both/nonprimary.swift

// REQUIRES: concurrency
// REQUIRES: swift_feature_DefaultIsolationPerFile

//--- dir_np_respected/plain.swift
// PRIMARY
// Error fires iff non-primary's `using` was honored.
nonisolated func callerInPlain() {
  TypeInWithUsing.staticMethod() // expected-error {{main actor-isolated}}
}

//--- dir_np_respected/withUsing.swift
// NON-PRIMARY
using @MainActor

class TypeInWithUsing {
  static func staticMethod() {} // expected-note {{calls to static method 'staticMethod()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@-1 {{main actor isolation inferred from file-level default isolation}}
}

//--- dir_p_no_bleed/withUsing.swift
// PRIMARY
using @MainActor

// Unexpected error fires iff primary's `using` bled into plain.swift.
nonisolated func callerInWithUsing() {
  TypeInPlain.staticMethod()
}

//--- dir_p_no_bleed/plain.swift
// NON-PRIMARY
class TypeInPlain {
  static func staticMethod() {}
}

//--- dir_np_no_bleed/plain.swift
// PRIMARY
// Calls MainActor from unannotated. Error fires iff no bleed.
func callerInPlainUnannotated() { // expected-note {{add '@MainActor' to make global function 'callerInPlainUnannotated()' part of global actor 'MainActor'}}
  TypeInWithUsingExplicit.explicitMainActorMethod() // expected-error {{main actor-isolated}}
}

//--- dir_np_no_bleed/withUsing.swift
// NON-PRIMARY
using @MainActor

class TypeInWithUsingExplicit {
  @MainActor static func explicitMainActorMethod() {} // expected-note {{calls to static method 'explicitMainActorMethod()' from outside of its actor context are implicitly asynchronous}}
}

//--- dir_both/primary.swift
// PRIMARY
using @MainActor

nonisolated func callerInBothPrimary() {
  TypeInBothNonprimary.staticMethod() // expected-error {{main actor-isolated}}
}

//--- dir_both/nonprimary.swift
// NON-PRIMARY
using @MainActor

class TypeInBothNonprimary {
  static func staticMethod() {} // expected-note {{calls to static method 'staticMethod()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@-1 {{main actor isolation inferred from file-level default isolation}}
}
