// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-sil -verify %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-sil -verify %s -strict-concurrency=complete

// REQUIRES: concurrency

struct UniqueResource: ~Copyable {
  let value: Int
}

func consumeIt(_ resource: consuming UniqueResource) {}

@available(SwiftStdlib 6.4, *)
func doubleResumeReturning() async {
  let _: Int = await withContinuation { c in // expected-error{{'c' consumed more than once}}
    c.resume(returning: 1) // expected-note{{consumed here}}
    c.resume(returning: 2) // expected-note{{consumed again here}}
  }
}

@available(SwiftStdlib 6.4, *)
func doubleResumeVoid() async {
  let _: Void = await withContinuation { c in // expected-error{{'c' consumed more than once}}
    c.resume(returning: ()) // expected-note{{consumed here}}
    c.resume(returning: ()) // expected-note{{consumed again here}}
  }
}

@available(SwiftStdlib 6.4, *)
func doubleResumeThrowing() async throws {
  let _: Int = try await withContinuation(of: Int.self, throwing: (any Error).self) { c in // expected-error{{'c' consumed more than once}}
    c.resume(returning: 1) // expected-note{{consumed here}}
    c.resume(returning: 2) // expected-note{{consumed again here}}
  }
}

@available(SwiftStdlib 6.4, *)
func doubleResumeMixed() async throws {
  struct MyError: Error {}
  let _: Int = try await withContinuation(of: Int.self, throwing: (any Error).self) { c in // expected-error{{'c' consumed more than once}}
    c.resume(returning: 1) // expected-note{{consumed here}}
    c.resume(throwing: MyError()) // expected-note{{consumed again here}}
  }
}

@available(SwiftStdlib 6.4, *)
func cannotCopyContinuation() async {
  let _: Int = await withContinuation { c in // expected-error{{'c' consumed more than once}}
    let c2 = c // expected-note{{consumed here}}
    c2.resume(returning: 1)
    c.resume(returning: 2) // expected-note{{consumed again here}}
  }
}

@available(SwiftStdlib 6.4, *)
func resumeAfterBranch(_ condition: Bool) async {
  let _: Int = await withContinuation { c in // expected-error{{'c' consumed more than once}}
    if condition {
      c.resume(returning: 1) // expected-note{{consumed here}}
    }
    c.resume(returning: 2) // expected-note{{consumed again here}}
  }
}

@available(SwiftStdlib 6.4, *)
func noncopyableDoubleResumeThrowing() async throws {
  let _: UniqueResource = try await withContinuation(of: UniqueResource.self, throwing: (any Error).self) { c in
    let resource = UniqueResource(value: 1) // expected-error{{'resource' consumed more than once}}
    c.resume(returning: resource) // expected-note{{consumed here}}
    consumeIt(resource) // expected-note{{consumed again here}}
  }
}
