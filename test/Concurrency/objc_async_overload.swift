// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk)  -disable-availability-checking -typecheck -verify -import-objc-header %S/Inputs/Delegate.h %s
// REQUIRES: concurrency
// REQUIRES: objc_interop


// overload resolution should pick sync version in a sync context
func syncContext() {
  let r = Request()
  let d = Delegate()
  d.makeRequest1(r) // NOTE: this use to trigger an overload resolution error, see SR-13760
  d.makeRequest2(r)
  d.makeRequest3(r)
}

// overload resolution should pick async version in an async context
func asyncNoAwait() async {
  let r = Request()
  let d = Delegate()
  d.makeRequest1(r) // expected-error@:3 {{expression is 'async' but is not marked with 'await'}} expected-note {{call is 'async'}}
  d.makeRequest2(r) // expected-error@:3 {{expression is 'async' but is not marked with 'await'}} expected-note {{call is 'async'}}
  d.makeRequest3(r) // expected-error@:3 {{expression is 'async' but is not marked with 'await'}} expected-note {{call is 'async'}}
}


func asyncWithAwait() async {
  let r = Request()
  let d = Delegate()
  await d.makeRequest1(r)
  await d.makeRequest2(r)
  await d.makeRequest3(r)
}
