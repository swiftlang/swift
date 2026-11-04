// RUN: %target-swift-frontend -typecheck -verify %s
// RUN: %target-swift-frontend -typecheck -verify -verify-additional-prefix clangfn- %s -use-clang-function-types

// Check that we catch various mismatches that TypeMatcher historically failed
// to notice. Most of these setups have two test cases--one using a generic
// function, the other using an associated type--because these exercise
// different parts of the generic system.

// Inverse requirements
protocol RequiresAny { associatedtype A where A == Any }
protocol RequiresNoncopyableAny { associatedtype A where A == any ~Copyable }

func conflictingInverses<T: RequiresAny & RequiresNoncopyableAny>(_ t: T) {}
// expected-error@-1 {{no type for 'T.A' can satisfy both 'T.A == any ~Copyable' and 'T.A == Any'}}

protocol ConflictingInversesViaMerge {
  // expected-error@-1 {{no type for 'Self.MergedA.A' can satisfy both 'Self.MergedA.A == any ~Copyable' and 'Self.MergedA.A == Any'}}
  associatedtype MergedA : RequiresAny & RequiresNoncopyableAny
}

// Function representation
protocol RequiresPlainConvention { associatedtype A where A == () -> Void }
protocol RequiresCConvention { associatedtype A where A == (@convention(c) () -> Void) }

func conflictingRepresentation<T: RequiresPlainConvention & RequiresCConvention>(_ t: T) {}
// expected-error@-1 {{no type for 'T.A' can satisfy both 'T.A == () -> ()' and 'T.A == @convention(c) () -> ()'}}

protocol ConflictingRepresentationViaMerge {
  // expected-error@-1 {{no type for 'Self.MergedA.A' can satisfy both 'Self.MergedA.A == () -> ()' and 'Self.MergedA.A == @convention(c) () -> ()'}}
  associatedtype MergedA : RequiresPlainConvention & RequiresCConvention
}

// Function async effect
protocol RequiresSync { associatedtype A where A == () -> Void }
protocol RequiresAsync { associatedtype A where A == (() async -> Void) }

func conflictingAsync<T: RequiresSync & RequiresAsync>(_ t: T) {}
// expected-error@-1 {{no type for 'T.A' can satisfy both 'T.A == () -> ()' and 'T.A == () async -> ()'}}

protocol ConflictingAsyncViaMerge {
  // expected-error@-1 {{no type for 'Self.MergedA.A' can satisfy both 'Self.MergedA.A == () -> ()' and 'Self.MergedA.A == () async -> ()'}}
  associatedtype MergedA : RequiresSync & RequiresAsync
}

// `sending` function result
protocol RequiresPlainResult { associatedtype A where A == () -> Int }
protocol RequiresSendingResult { associatedtype A where A == (() -> sending Int) }

func conflictingSendingResult<T: RequiresPlainResult & RequiresSendingResult>(_ t: T) {}
// expected-error@-1 {{no type for 'T.A' can satisfy both 'T.A == () -> sending Int' and 'T.A == () -> Int'}}

protocol ConflictingSendingResultViaMerge {
  // expected-error@-1 {{no type for 'Self.MergedA.A' can satisfy both 'Self.MergedA.A == () -> sending Int' and 'Self.MergedA.A == () -> Int'}}
  associatedtype MergedA : RequiresPlainResult & RequiresSendingResult
}

// Function underlying C type (only diagnosed with '-use-clang-function-types')
protocol RequiresPlainCType { associatedtype A where A == (@convention(c) (Int32) -> Int32) }
protocol RequiresExplicitCType { associatedtype A where A == (@convention(c, cType: "long (*)(long)") (Int32) -> Int32) }

func conflictingCType<T: RequiresPlainCType & RequiresExplicitCType>(_ t: T) {}
// expected-clangfn-error@-1 {{no type for 'T.A' can satisfy both 'T.A == @convention(c) (Int32) -> Int32' and 'T.A == @convention(c) (Int32) -> Int32'}}

protocol ConflictingCTypeViaMerge {
  // expected-clangfn-error@-1 {{no type for 'Self.MergedA.A' can satisfy both 'Self.MergedA.A == @convention(c) (Int32) -> Int32' and 'Self.MergedA.A == @convention(c) (Int32) -> Int32'}}
  associatedtype MergedA : RequiresPlainCType & RequiresExplicitCType
}

// Function isolation
protocol RequiresNonisolated { associatedtype A where A == () -> Void }
protocol RequiresMainActor { associatedtype A where A == (@MainActor () -> Void) }

func conflictingIsolation<T: RequiresNonisolated & RequiresMainActor>(_ t: T) {}
// expected-error@-1 {{no type for 'T.A' can satisfy both 'T.A == () -> ()' and 'T.A == @MainActor () -> ()'}}

protocol ConflictingIsolationViaMerge {
  // expected-error@-1 {{no type for 'Self.MergedA.A' can satisfy both 'Self.MergedA.A == () -> ()' and 'Self.MergedA.A == @MainActor () -> ()'}}
  associatedtype MergedA : RequiresNonisolated & RequiresMainActor
}

// Edge case which would fail if we simply checked actor isolation using type
// equality, rather than a semantic match
@globalActor
actor ActorA<T> {
  @diagnose(UnstableGlobalActorShared, as: ignored)
  static var shared: ActorA<T> { ActorA<T>() }
}

protocol RequiresActor1 {
  associatedtype Act1
  associatedtype T where T == @ActorA<Act1> (Int) -> Void
}

protocol RequiresActor2 {
  associatedtype Act2
  associatedtype T where T == @ActorA<Act2> (Int) -> Void
}

protocol NonconflictingIsolationViaMerge {
  associatedtype T : RequiresActor1 & RequiresActor2 where T.Act1 == T.Act2
}
