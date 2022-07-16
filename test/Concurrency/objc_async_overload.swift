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

// rdar://88703266 - Swift 5 mode should warn, not error, if an imported
// completion handler's implicit `@Sendable` isn't respected.
extension Delegate {
  nonisolated func makeRequest(_ req: Request??, completionHandler: (() -> Void)? = nil) {
    // expected-DISABLED-note@-1 {{parameter 'completionHandler' is implicitly non-sendable}}
    if let req = (req ?? nil) {
      makeRequest1(req, completionHandler: completionHandler)
      // expected-DISABLED-warning@-1 {{passing non-sendable parameter 'completionHandler' to function expecting a @Sendable closure}}
    }
  }
}

@MainActor class C {
  func finish() { }
  // expected-DISABLED-note@-1 {{calls to instance method 'finish()' from outside of its actor context are implicitly asynchronous}}

  func handle(_ req: Request, with delegate: Delegate) {
    delegate.makeRequest1(req) {
      self.finish()
      // expected-DISABLED-warning@-1 {{call to main actor-isolated instance method 'finish()' in a synchronous nonisolated context; this is an error in Swift 6}}
    }
  }
}

// rdar://95887113 - Implementing an ObjC category method in Swift is not strictly valid, but should be tolerated

extension Delegate {
  @objc public func makeRequest(fromSwift: Request, completionHandler: (() -> Void)?) {}
}
