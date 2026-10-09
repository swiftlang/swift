// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -target %target-swift-6.2-abi-triple \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes

// REQUIRES: concurrency
// REQUIRES: swift_feature_Lifetimes

// A closure literal passed directly to withoutActuallyEscaping is
// nonescaping, so it may capture a ~Escapable value, just like a closure
// passed to a nonescaping parameter that is then passed to
// withoutActuallyEscaping (https://github.com/swiftlang/swift/issues/93107).

struct Owner: ~Copyable {
  var fd: Int32
}

struct Half: ~Copyable, ~Escapable, Sendable {
  let fd: Int32

  @_lifetime(borrow owner)
  init(_ owner: borrowing Owner) { fd = owner.fd }

  func use() -> Int32 { fd }
  func useAsync() async -> Int32 { fd }
}

func takeEscaping(_ f: @escaping () -> Void) {}

func captureInClosureLiteral(_ owner: borrowing Owner) {
  let half = Half(owner)
  withoutActuallyEscaping({ _ = half.use() }) { body in
    body()
  }
}

func captureInClosureLiteralWithCaptureList(_ owner: borrowing Owner) {
  let half = Half(owner)
  withoutActuallyEscaping({ [half] in _ = half.use() }) { body in
    body()
  }
}

func captureInClosureLiteralReturningValue(_ owner: borrowing Owner) -> Int32 {
  let half = Half(owner)
  return withoutActuallyEscaping({ half.use() }) { body in
    body()
  }
}

func captureInClosureLiteralTaskGroup(_ owner: borrowing Owner) async {
  let half = Half(owner)
  await withoutActuallyEscaping({ @Sendable in _ = await half.useAsync() }) {
    body in
    await withTaskGroup { group in
      group.addTask { await body() }
    }
  }
}

// The same code split into a helper has always been accepted.
func concurrently(_ body: @Sendable () async -> Void) async {
  await withoutActuallyEscaping(body) { body in
    await withTaskGroup { group in
      group.addTask { await body() }
    }
  }
}

func captureViaHelper(_ owner: borrowing Owner) async {
  let half = Half(owner)
  await concurrently { _ = await half.useAsync() }
}

// A closure stored in a local variable is escaping, so capturing a ~Escapable
// value is still an error.
func captureInEscapingLocal(_ owner: borrowing Owner) {
  let half = Half(owner) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{this use causes the lifetime-dependent value to escape}}
  // expected-note @-3 {{it depends on the lifetime of argument 'owner'}}
  let body: () -> Void = { _ = half.use() }
  withoutActuallyEscaping(body) { body in
    body()
  }
}
