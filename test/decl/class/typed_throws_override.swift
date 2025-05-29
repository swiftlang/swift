// RUN: %target-typecheck-verify-swift -parse-as-library

enum MyError: Error {
case failed
}

enum HomeworkError: Error {
case dogAteIt
}

class SuperError: Error { }
class SubError: SuperError { }

class Super {
  func f() throws { }
  func g() throws(MyError) { }
  func h() throws(HomeworkError) { } // expected-note{{overridden declaration is here}}
  func i() throws(SuperError) { }

  var propF: Int {
    get throws { 5 }
  }

  var propG: Int {
    get throws(MyError) { 5 }
  }

  var propH: Int {
    get throws(HomeworkError) { 5 } // expected-note{{overridden declaration is here}}
  }

  var propI: Int {
    get throws(SuperError) { 5 }
  }
}

class Sub: Super {
  override func f() throws(MyError) { } // okay to make type more specific
  override func g() { } // okay to be non-throwing
  override func h() throws(MyError) { } // expected-error{{instance method that throws 'MyError' cannot override one that throws 'HomeworkError'}}
  override func i() throws(SubError) { } // okay to have a subtype

  override var propF: Int {
    get throws(MyError) { 5 } // okay to make type more specific
  }

  override var propG: Int {
    get { 5 } // okay to be non-throwing
  }

  override var propH: Int {
    get throws(MyError) { 5 } // expected-error{{getter that throws 'MyError' cannot override one that throws 'HomeworkError'}}
  }

  override var propI: Int {
    get throws(SubError) { 5 } // okay to make type more specific
  }
}
