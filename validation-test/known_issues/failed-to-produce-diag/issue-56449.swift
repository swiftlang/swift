// rdar://74111727 (https://github.com/swiftlang/swift/issues/56449)
// RUN: %target-typecheck-verify-swift

func cf2<K>(_ f: (K) -> Void) {}
func cf1<K>(_ f: @convention(thin) (K) -> Void) {}

func a() -> @convention(thin) (Int) -> Int { fatalError() }
func a() -> @convention(c) (Int) -> Int { fatalError() }
func a() -> @convention(swift) (Int) -> Int { fatalError() }

cf2(a()) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
cf1(a()) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
