// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -typecheck -swift-version 6 -parse-as-library -verify %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_DefaultIsolationPerFile

using nonisolated

// implicitly Sendable
@MainActor class Base {}

final class FinalSub: Base {}

// TODO: diagnostic would ideally provide more information in these cases...

// implicitly nonisolated
class Sub: Base {} // expected-error {{non-final class 'Sub' cannot conform to the 'Sendable' protocol}}

// implicitly nonisolated
// nonisolated subclass can't inherit Sendable from @MainActor even with @unchecked
class UncheckedSub: Base, @unchecked Sendable {} // expected-error {{non-final class 'UncheckedSub' cannot conform to the 'Sendable' protocol}}
