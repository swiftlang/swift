// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -verify %s -o /dev/null -swift-version 6

// REQUIRES: concurrency

// A live Span is a use of the storage it borrows. Sending the source var
// while that Span is still used should be diagnosed.
// https://github.com/swiftlang/swift/issues/89904

func spanUseAfterSend() async {
  var a = [1.0, 2.0, 3.0]
  let s = a.span
  Task { // expected-error {{closure passed as an argument to a 'sending' parameter captures reference to mutable var 'a' which is accessed later by code in the current isolation context}}
    a = []
  }
  _ = s[0] // expected-note {{access can happen concurrently}}
}

func spanUseAfterAwaitedSend() async {
  var a = [1.0, 2.0, 3.0]
  let s = a.span
  await Task { // expected-error {{closure passed as an argument to a 'sending' parameter captures reference to mutable var 'a' which is accessed later by code in the current isolation context}}
    a = []
  }.value
  _ = s[0] // expected-note {{access can happen concurrently}}
}

@MainActor
func spanUseAfterSendSameIsolation() async {
  var a = [1.0, 2.0, 3.0]
  let s = a.span
  await Task { @MainActor in // expected-error {{closure passed as an argument to a 'sending' parameter captures reference to mutable var 'a' which is accessed later by code in the current isolation context}}
    a.append(4.0)
  }.value
  _ = s[0] // expected-note {{access can happen concurrently}}
}

func derivedSpanUseAfterSend() async {
  var a = [1.0, 2.0, 3.0]
  let s = a.span
  let tail = s.extracting(droppingFirst: 1)
  await Task { // expected-error {{closure passed as an argument to a 'sending' parameter captures reference to mutable var 'a' which is accessed later by code in the current isolation context}}
    a = []
  }.value
  _ = tail[0] // expected-note {{access can happen concurrently}}
}

func spanDeadBeforeSend() async {
  var a = [1.0, 2.0, 3.0]
  do {
    let s = a.span
    _ = s[0]
  }
  Task {
    a = []
  }
}
