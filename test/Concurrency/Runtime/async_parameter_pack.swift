// RUN: %target-run-simple-swift( -target %target-swift-5.9-abi-triple)
// REQUIRES: executable_test
// REQUIRES: concurrency

protocol P {
  associatedtype A
  var a: A { get }
}

func f<each T: P>(_ t: repeat each T) async -> (repeat (each T).A) {
  let x = (repeat (each t).a)
  return x
}

struct S: P {
  var a: String { "" }
}

_ = await f()
_ = await f(S())
_ = await f(S(), S())
