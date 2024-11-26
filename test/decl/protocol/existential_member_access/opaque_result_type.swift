// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

protocol P {}
extension P {
  func method() -> some P { self }
  var property: some P { self }
  subscript(_: Void) -> some P { self }
}

do {
  let exist: any P

  let _: any P = exist.method()
  let _: any P = exist.property
  let _: any P = exist[()]
}
