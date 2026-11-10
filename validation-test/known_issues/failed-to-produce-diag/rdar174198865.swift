// rdar://174198865
// RUN: %target-typecheck-verify-swift

func takeCallback(
  _ cb: (@convention(c) (OpaquePointer?, OpaquePointer?) -> Int32)?
) {}

func callback(_: UnsafeMutablePointer<Int32>?, _: OpaquePointer?) -> Int32 {
  0
}

func test() {
  takeCallback(callback) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
