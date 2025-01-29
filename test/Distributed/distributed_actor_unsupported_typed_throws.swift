// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

distributed actor Foo {
  distributed func alwaysThrows() throws(FooError) { // expected-error{{cannot declare distributed function with typed throws}}
    throw FooError()
  }
}

struct FooError: Codable, Error { }
struct RemoteInvocationError: Codable, Error { }

func test(foo: Foo) async throws {
  do {
    try await foo.alwaysThrows() // actually, this is throws(Error) because network errors etc
    fatalError("Should not reach here")
    // FIXME: the following warning is showing why we had to ban typed throws;
    //        the error type must instead be (FooError | Error (from the distributed thunk))
    //        rdar://136467591
  } catch let error as RemoteInvocationError { // expected-warning{{cast from 'FooError' to unrelated type 'RemoteInvocationError' always fails}}
    print("error = \(error)")
  }
}