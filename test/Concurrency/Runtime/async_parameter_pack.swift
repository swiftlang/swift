// RUN: %target-run-simple-swift( -target %target-swift-5.9-abi-triple)
// RUN: %target-run-simple-swift( -target %target-swift-5.9-abi-triple -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault)
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault
// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: back_deployment_runtime || use_os_stdlib

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
