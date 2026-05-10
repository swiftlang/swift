// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk)  -target %target-swift-5.1-abi-triple -emit-sil -o /dev/null -verify -import-objc-header %S/Inputs/Delegate.h -enable-experimental-feature SendableCompletionHandlers %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk)  -target %target-swift-5.1-abi-triple -emit-sil -o /dev/null -verify -import-objc-header %S/Inputs/Delegate.h -enable-experimental-feature SendableCompletionHandlers %s -strict-concurrency=targeted
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk)  -target %target-swift-5.1-abi-triple -emit-sil -o /dev/null -verify -import-objc-header %S/Inputs/Delegate.h -enable-experimental-feature SendableCompletionHandlers %s -strict-concurrency=complete

// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: swift_feature_SendableCompletionHandlers

// overload resolution should pick sync version in a sync context
func syncContext() {
  let r = Request()
  let d = Delegate()

  // https://github.com/apple/swift/issues/56157
  // This use to trigger an overload resolution error.
  d.makeRequest1(r)

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
    // expected-note@-1 {{parameter 'completionHandler' is implicitly non-Sendable}}
    if let req = (req ?? nil) {
      makeRequest1(req, completionHandler: completionHandler)
      // expected-warning@-1 {{passing non-Sendable parameter 'completionHandler' to function expecting a '@Sendable' closure}}
    }
  }
}

@MainActor class C {
  func finish() { }
  // expected-note@-1 {{calls to instance method 'finish()' from outside of its actor context are implicitly asynchronous}}

  func handle(_ req: Request, with delegate: Delegate) {
    delegate.makeRequest1(req) {
      self.finish()
      // expected-warning@-1 {{call to main actor-isolated instance method 'finish()' in a synchronous nonisolated context}}
    }
  }
}

// rdar://95887113 - Implementing an ObjC category method in Swift is not strictly valid, but should be tolerated

extension Delegate {
  @objc public func makeRequest(fromSwift: Request, completionHandler: (() -> Void)?) {}
}
