// rdar://175211520 (https://github.com/swiftlang/swift/issues/88575)
// RUN: %target-typecheck-verify-swift

struct Point {
  static var zero: Point { Point() }
  static prefix func - (p: Point) -> Point { p }
}

func getPoints() -> [Point]? { nil }

func nonOptional() -> Point { getPoints()?.map { -$0 } }
// expected-error@-1 {{cannot convert return expression of type '[Point]?' to return type 'Point'}}

func viaLocal() -> Point? {
  let value = getPoints()?.map { -$0 }
  return value
  // expected-error@-1 {{cannot convert return expression of type '[Point]?' to return type 'Point?'}}
  // expected-note@-2 {{arguments to generic parameter 'Wrapped' ('[Point]' and 'Point') are expected to be equal}}
}

func optionalChain() -> Point? { getPoints()?.map { -$0 } }
// expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}

func optionalChainOnNonOptional() -> Point? { [Point.zero]?.map { -$0 } }
// expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
