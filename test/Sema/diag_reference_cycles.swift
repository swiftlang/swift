// RUN: %target-typecheck-verify-swift

class Test0 {
  var closure: () -> () = { }
  func actNow() {}
}

func test(x : Test0) {
  x.closure = { // expected-warning {{implicitly capturing 'x' strongly in this closure is likely to lead to a reference cycle}}
    // expected-note@-1 {{closure is strongly referenced by an object strongly referenced by 'x'}}
    // expected-note@-2 {{capture 'x' explicitly to silence this warning}} {{16-16= [x] in}}
    x.actNow()
  }

  x.closure = { [x] in
    x.actNow()
  }
}

class ClosureOwner {
  var strongClosure: () -> () = {}
}

class Test1 {
  var owner = ClosureOwner()
  unowned var owner2: ClosureOwner
  weak var owner3: ClosureOwner?

  init() {
    self.owner2 = self.owner
    self.owner3 = self.owner
  }
}

func test(x : Test1) {
  x.owner.strongClosure = { // expected-warning {{implicitly capturing 'x' strongly in this closure is likely to lead to a reference cycle}}
    // expected-note@-1 {{closure is strongly referenced by an object strongly referenced by 'x'}}
    // expected-note@-2 {{capture 'x' explicitly to silence this warning}} {{28-28= [x] in}}
    _ = x
  }
  x.owner2.strongClosure = {
    _ = x
  }
  x.owner3!.strongClosure = {
    _ = x
  }

  x.owner.strongClosure = { [x] in
    _ = x
  }
  x.owner2.strongClosure = { [x] in
    _ = x
  }
  x.owner3!.strongClosure = { [x] in
    _ = x
  }
}

func test2_helper(_ x : AnyObject) {}

class Test2 {
  var closure: () -> () = {}
  let x: AnyObject = Test0()

  func test() {
    self.closure = { // expected-warning {{implicitly capturing 'self' strongly in this closure is likely to lead to a reference cycle}}
      // expected-note@-1 {{closure is strongly referenced by an object strongly referenced by 'self'}}
      // expected-note@-2 {{capture 'self' explicitly to silence this warning}} {{21-21= [self] in}}
      test2_helper(self.x)
    }

    self.closure = { [self] in
      test2_helper(x)
    }

    self.closure = { [self] in
      test2_helper(self.x)
    }
  }
}

class Test3 {
  func doNoEscapeClosure(_ f: () -> ()) {}
  func doEscapingClosure(_ f: @escaping () -> ()) {}
  func doAutoclosure(_ f: @autoclosure () -> Test3) {}
  func doEscapingAutoclosure(_ f: @escaping @autoclosure () -> Test3) {}

  func test(_ x: Test3) {
    x.doNoEscapeClosure {
      _ = x
    }

    x.doEscapingClosure { // expected-warning {{implicitly capturing 'x' strongly in this closure is likely to lead to a reference cycle}}
      // expected-note@-1 {{closure capturing 'x' is referenced as an escaping argument to this function call}}
      // expected-note@-2 {{capture 'x' explicitly to silence this warning}} {{26-26= [x] in}}
      _ = x
    }

    x.doEscapingClosure { [x] in
      _ = x
    }

    x.doAutoclosure(x)

    x.doEscapingAutoclosure(x) // expected-warning {{implicitly capturing 'x' strongly in this closure is likely to lead to a reference cycle}}
      // expected-note@-1 {{closure capturing 'x' is referenced as an escaping argument to this function call}}
  }
}

class HTMLElement {
  let name: String
  let text: String?

  // Unapplied closures escape
  lazy var asHTML: () -> String = { // expected-warning {{implicitly capturing 'self' strongly in this closure is likely to lead to a reference cycle}}
    // expected-note@-1 {{closure is strongly referenced by an object strongly referenced by 'self'}}
    // expected-note@-2 {{capture 'self' explicitly to silence this warning}} {{36-36= [self] in}}
    if let text = self.text {
      return "<\(self.name)>\(text)</\(self.name)>"
    } else {
      return "<\(self.name) />"
    }
  }

  // Unapplied closures escape
  lazy var asHTMLNoCapture: () -> String = { [self] in
    if let text = self.text {
      return "<\(self.name)>\(text)</\(self.name)>"
    } else {
      return "<\(self.name) />"
    }
  }

  lazy var asHTMLWeakify: () -> String = { [weak self] in // No warning
    if let text = self!.text {
      return "<\(self!.name)>\(text)</\(self!.name)>"
    } else {
      return "<\(self!.name) />"
    }
  }

  // Immediately-applied closures are no-escape.
  lazy var asHTMLNoEscape: String = { // No warning
    if let text = self.text {
      return "<\(self.name)>\(text)</\(self.name)>"
    } else {
      return "<\(self.name) />"
    }
  }()

  init(name: String, text: String? = nil) {
    self.name = name
    self.text = text
  }
}

