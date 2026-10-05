// RUN: %swift -typecheck -verify -target %target-cpu-apple-macosx11 %s

// REQUIRES: OS=macosx

@available(macOS 99, *)
enum MyError: Error {
  case fail
}

@available(macOS 12, *)
func throwMyErrorBadly() throws(MyError) { }
// expected-error@-1{{'MyError' is only available in macOS 99 or newer}}

// The conformance of the thrown error type to 'Error' must be available too.

struct WillBeError { }

@available(macOS 99, *)
extension WillBeError: Error { }

func throwsWillBeError() throws(WillBeError) { }
// expected-warning@-1{{conformance of 'WillBeError' to 'Error' is only available in macOS 99 or newer}}
// expected-note@-2{{add '@available' attribute to enclosing global function}}

@available(macOS 99, *)
func guardedThrowsWillBeError() throws(WillBeError) { }

struct HasInit { // expected-note{{add '@available' attribute to enclosing struct}}
  init() throws(WillBeError) { }
  // expected-warning@-1{{conformance of 'WillBeError' to 'Error' is only available in macOS 99 or newer}}
  // expected-note@-2{{add '@available' attribute to enclosing initializer}}
}

protocol P { // expected-note{{add '@available' attribute to enclosing protocol}}
  func requirement() throws(WillBeError)
  // expected-warning@-1{{conformance of 'WillBeError' to 'Error' is only available in macOS 99 or newer}}
  // expected-note@-2{{add '@available' attribute to enclosing instance method}}
}

func takesFunction(_ fn: () throws(WillBeError) -> Void) { }
// expected-warning@-1{{conformance of 'WillBeError' to 'Error' is only available in macOS 99 or newer}}
// expected-note@-2{{add '@available' attribute to enclosing global function}}

func returnsFunction() -> (() throws(WillBeError) -> Void)? { nil }
// expected-warning@-1{{conformance of 'WillBeError' to 'Error' is only available in macOS 99 or newer}}
// expected-note@-2{{add '@available' attribute to enclosing global function}}

var storedFunction: (() throws(WillBeError) -> Void)? = nil
// expected-warning@-1{{conformance of 'WillBeError' to 'Error' is only available in macOS 99 or newer}}
// expected-note@-2{{add 'if #available' version check}}

func localFunctionType() { // expected-note{{add '@available' attribute to enclosing global function}}
  let fn: () throws(WillBeError) -> Void = { () throws(WillBeError) in }
  // expected-warning@-1{{conformance of 'WillBeError' to 'Error' is only available in macOS 99 or newer}}
  // expected-note@-2{{add 'if #available' version check}}
  _ = fn
}

struct GenericError<T> { }

@available(macOS 99, *)
extension GenericError: Error where T: Equatable { }

func throwsGenericError<U: Equatable>(_: U.Type) throws(GenericError<U>) { }
// expected-warning@-1{{conformance of 'GenericError<T>' to 'Error' is only available in macOS 99 or newer}}
// expected-note@-2{{add '@available' attribute to enclosing global function}}

// The conformance of the thrown error type may be available while a conformance
// that it depends on is not.

struct Box<T> { }

extension Box: Error where T: Error { }

func throwsBox() throws(Box<WillBeError>) { }
// expected-warning@-1{{conformance of 'WillBeError' to 'Error' is only available in macOS 99 or newer}}
// expected-note@-2{{add '@available' attribute to enclosing global function}}

// An unavailable conformance is an error, not a warning.

struct NeverError { }

@available(*, unavailable)
// expected-note@-1{{conformance of 'NeverError' to 'Error' has been explicitly marked unavailable here}}
extension NeverError: Error { }

func throwsNeverError() throws(NeverError) { }
// expected-error@-1{{conformance of 'NeverError' to 'Error' is unavailable}}

// A thrown error type that is itself unavailable is diagnosed on its own. Its
// conformance is not diagnosed a second time.

@available(macOS 99, *)
struct UnavailableError { }

@available(macOS 99, *)
extension UnavailableError: Error { }

func throwsUnavailableError() throws(UnavailableError) { }
// expected-error@-1{{'UnavailableError' is only available in macOS 99 or newer}}
// expected-note@-2{{add '@available' attribute to enclosing global function}}
