
// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/50419

func f<T>(_ x: T) -> T {
  return x
}

func f<T>(_ x: T?) -> T? {
  return x
}

let r = f(1)
let _ = r! // expected-error {{cannot force unwrap value of non-optional type 'Int'}}

// https://github.com/apple/swift/issues/50434

func testLazySequence(_ lazySequence: LazySequence<[Int]>?) {
  let value = lazySequence?.compactMap({ $0 as? Int }).first // expected-warning {{conditional cast from 'Int' to 'Int' always succeeds}}
  let _: Int = value!
}
