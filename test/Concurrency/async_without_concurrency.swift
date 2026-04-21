// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-ir -o - -disable-implicit-concurrency-module-import %s -verify

// REQUIRES: concurrency

struct ConcurrencyTest {
  func test() async {  // expected-error{{'_Concurrency' module not imported, required for async}}
  }

  func test2() {
    _ = { () async in  // expected-error{{'_Concurrency' module not imported, required for async}}
    }
  }
}

typealias Fn = () async -> String // expected-error{{'_Concurrency' module not imported, required for async}}
