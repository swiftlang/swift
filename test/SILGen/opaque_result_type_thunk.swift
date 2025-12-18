// RUN: %target-swift-emit-silgen %s

// https://github.com/swiftlang/swift/issues/86118

// The AST type of the thunk depends on the generic signature, but the
// lowered type does not, because we can see the opaque return type's
// underlying type from the expansion point, and it does not involve
// type parameters. Make sure this does not cause us to assert.

public struct G<T> {
  public static func f(_: Any, _: Any) -> some Any {
    return 123
  }
}

public func g<T>(_: T) {
  let fn: (Any, Any) -> _ = G<T>.f
  let fn2: (Int, Int) -> _ = fn
}
