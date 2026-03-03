// RUN: %target-swift-frontend -strict-concurrency=targeted -swift-version 5 -parse-as-library -emit-sil -verify %s

@_nonSendable
class NonSendableType {
  var x: Int = 0
  func f() {}
}

// rdar://94699928 - don't emit sendable diagnostics in non-'complete' mode
// for deinits of actor- or global-actor-isolated types

@available(SwiftStdlib 5.1, *)
@MainActor class AwesomeUIView {}

@available(SwiftStdlib 5.1, *)
class CheckDeinitFromClass: AwesomeUIView {
  var ns: NonSendableType?
  deinit {
    ns?.f()
    ns = nil
  }
}

@available(SwiftStdlib 5.1, *)
actor CheckDeinitFromActor {
  var ns: NonSendableType?
  deinit {
    ns?.f()
    ns = nil
  }
}

@available(SwiftStdlib 5.1, *)
@MainActor class X {
  var ns: NonSendableType = NonSendableType()

  nonisolated func something() { }

  deinit {
    something()
    print(ns)
  }
}
