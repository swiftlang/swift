// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -swift-version 6 -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -parse-as-library %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// A 'send' call always consumes its noncopyable arguments, whether the
// parameter is 'consuming' or 'borrowing': the value must outlive the caller's
// scope. A 'borrowing' parameter borrows the value for the call on the actor,
// and the value is destroyed once the call returned. Using the argument after
// the 'send' is a use after consume

struct FileHandle: ~Copyable {
  var fd: Int
  func write() {}
}

actor Logger {
  // Synchronous callees, which are enqueued on the actor
  func retarget(to handle: consuming FileHandle) {}
  func analyze(_ handle: borrowing FileHandle) {}
  func both(_ a: borrowing FileHandle, _ b: consuming FileHandle) {}

  // 'async' callees, which run in a task
  func retargetAsync(to handle: consuming FileHandle) async {}
  func analyzeAsync(_ handle: borrowing FileHandle) async {}
}

func consumes(_ logger: Logger) {
  let a = FileHandle(fd: 1)
  send logger.retarget(to: a)
  let b = FileHandle(fd: 2)
  send logger.analyze(b)
  let c = FileHandle(fd: 3)
  send logger.retargetAsync(to: c)
  let d = FileHandle(fd: 4)
  send logger.analyzeAsync(d)
  send logger.both(FileHandle(fd: 5), FileHandle(fd: 6))
}

func consumesParameters(_ logger: Logger, _ a: consuming FileHandle,
                        _ b: consuming FileHandle) {
  send logger.retarget(to: a)
  send logger.analyzeAsync(b)
}

func useAfterSendToConsuming(_ logger: Logger) {
  let a = FileHandle(fd: 1) // expected-error{{'a' used after consume}}
  send logger.retarget(to: a) // expected-note{{consumed here}}
  a.write() // expected-note{{used here}}
}

func useAfterSendToBorrowing(_ logger: Logger) {
  let b = FileHandle(fd: 2) // expected-error{{'b' used after consume}}
  send logger.analyze(b) // expected-note{{consumed here}}
  b.write() // expected-note{{used here}}
}

func useAfterSendToConsumingAsync(_ logger: Logger) {
  let c = FileHandle(fd: 3) // expected-error{{'c' used after consume}}
  send logger.retargetAsync(to: c) // expected-note{{consumed here}}
  c.write() // expected-note{{used here}}
}

func useAfterSendToBorrowingAsync(_ logger: Logger) {
  let d = FileHandle(fd: 4) // expected-error{{'d' used after consume}}
  send logger.analyzeAsync(d) // expected-note{{consumed here}}
  d.write() // expected-note{{used here}}
}

// A borrowed value cannot be consumed by a 'send', even for a 'borrowing'
// parameter. An 'await' only borrows it
func borrowedParameter(_ logger: Logger, _ h: borrowing FileHandle) async {
  // expected-error@-1{{'h' is borrowed and cannot be consumed}}
  send logger.analyze(h) // expected-note{{consumed here}}
  send logger.analyzeAsync(h) // expected-note{{consumed here}}
  await logger.analyze(h)
  await logger.analyzeAsync(h)
}
