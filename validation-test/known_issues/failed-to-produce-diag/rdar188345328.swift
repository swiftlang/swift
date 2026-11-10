// rdar://188345328
// RUN: %target-typecheck-verify-swift

func test(f: @escaping (UnsafeMutablePointer<Int>?) -> Void) {
  let _: (UnsafeMutablePointer<Float>?) -> Void = f
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}
