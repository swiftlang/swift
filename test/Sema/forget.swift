// RUN: %target-typecheck-verify-swift -enable-experimental-feature MoveOnlyEnumDeinits

// Typechecking for the forget statement.

func _forget() -> Int {}

let x = _forget()

_forget x // expected-error {{'forget' statement cannot appear in top-level code}}

func `_forget`(_ t: Int) {} // expected-note {{'_forget' declared here}}

func globalFunc() {
  _forget x // expected-error {{'forget' statement cannot appear in global function}}
}

class C {
  deinit {
    _forget self // expected-error {{'forget' statement cannot appear in deinitializer}}
  }
}

struct S {

  static func staticFunc() {
    _forget x // expected-error {{'forget' statement cannot appear in static method}}
  }

  var member: Int {
    get {
      _forget x // expected-error {{'forget' statement can only appear in noncopyable type's member}}
    }
  }

  __consuming func f() {
    _forget self // expected-error {{'forget' statement can only appear in noncopyable type's member}}
  }
}

enum E: Error { case err }

@_moveOnly struct File {
  let fd: Int

  init() throws {
    self.fd = 0

    _forget self // expected-error {{'forget' statement cannot appear in initializer}}

    throw E.err
  }

  init(_ b: Bool) throws {
    try self.init()

    _forget self // expected-error {{'forget' statement cannot appear in initializer}}

    throw E.err
  }

  __consuming func close() {
    defer { _forget self }

    if fd >= 0 {
      _forget self
      return
    }

    _forget self
  }

  var what: Int {
    __consuming get {
      _forget self
      return 0
    }
  }

  __consuming func badClose() {
    _forget self.fd // expected-error {{you can only forget 'self'}}{{13-20=self}}
    // expected-error@-1 {{cannot convert value of type 'Int' to expected forget type 'File'}}

    _forget Self // expected-error {{you can only forget 'self'}}{{13-17=self}}
    // expected-error@-1 {{cannot convert value of type 'File.Type' to expected forget type 'File'}}

    _forget Self.self // expected-error {{you can only forget 'self'}}{{13-22=self}}
    // expected-error@-1 {{cannot convert value of type 'File.Type' to expected forget type 'File'}}

    _forget self + self // expected-error {{you can only forget 'self'}}{{13-24=self}}
    // expected-error@-1 {{binary operator '+' cannot be applied to two 'File' operands}}

    // NOTE: I think this error comes from it trying to call _forget's subscript
    _forget [self]  // expected-error {{reference to member 'subscript' cannot be resolved without a contextual type}}

    _forget {} // expected-error {{trailing closure passed to parameter of type 'Int' that does not accept a closure}}

    _forget (self) // expected-error {{cannot convert value of type 'File' to expected argument type 'Int'}}

    // FIXME: we should get an error about it being illegal to forget in a closure.
    let _ = { // expected-error {{type of expression is ambiguous without more context}}
      _forget self
      return 0
    }()

    func hello() {
      _forget self // expected-error {{'forget' statement cannot appear in local function}}
    }
  }

  deinit {
    _forget self // expected-error {{'forget' statement cannot appear in deinitializer}}
  }

  static func staticFunc() {
    _forget x // expected-error {{'forget' statement cannot appear in static method}}
  }
}

@_moveOnly enum FileWrapper {
  case valid(File)
  case invalid(File)
  case nothing

  init() throws {
    _forget self // expected-error {{'forget' statement cannot appear in initializer}}
    throw E.err
  }

  __consuming func take() throws -> File {
    if case let .valid(f) = self {
      return f
    }
    _forget self
    throw E.err
  }

  var validFile: File {
    __consuming get {
      if case let .valid(f) = self {
        return f
      }
      _forget self
    }
  }

  deinit {
    try? take().close()
  }
}

struct NoDeinitStruct: ~Copyable {
  consuming func blah() {
    _forget self // expected-error {{'forget' has no effect for type 'NoDeinitStruct' unless it has a deinitializer}}{{5-18=}}
  }
}

enum NoDeinitEnum: ~Copyable {
  case whatever

  consuming func blah() {
    _forget self // expected-error {{'forget' has no effect for type 'NoDeinitEnum' unless it has a deinitializer}}{{5-18=}}
  }
}
