// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -swift-version 6 -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library %s

// REQUIRES: concurrency
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SendCalls

// A 'send' call consumes its noncopyable arguments, for a 'consuming' and a
// 'borrowing' parameter, and for a synchronous (enqueued) and an 'async'
// callee. A later use of the argument is a use after consume

import _Concurrency

struct Handle: ~Copyable {
  var id: Int
  func use() {}
}

actor Logger {
  func retarget(to handle: consuming Handle) {}
  func analyze(_ handle: borrowing Handle) {}
  func retargetAsync(to handle: consuming Handle) async {}
  func analyzeAsync(_ handle: borrowing Handle) async {}
}

func consumes(_ logger: Logger) {
  send logger.retarget(to: Handle(id: 1))
  let b = Handle(id: 2)
  send logger.analyze(b)
  send logger.retargetAsync(to: Handle(id: 3))
  let d = Handle(id: 4)
  send logger.analyzeAsync(d)
}

func useAfterConsume(_ logger: Logger) {
  let a = Handle(id: 1) // expected-error{{'a' used after consume}}
  send logger.retarget(to: a) // expected-note{{consumed here}}
  a.use() // expected-note{{used here}}

  let b = Handle(id: 2) // expected-error{{'b' used after consume}}
  send logger.analyze(b) // expected-note{{consumed here}}
  b.use() // expected-note{{used here}}
}
