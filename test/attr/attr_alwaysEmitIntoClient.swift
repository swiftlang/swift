// RUN: %target-typecheck-verify-swift

private func privateFunction() {}
// expected-note@-1{{global function 'privateFunction()' is not '@usableFromInline' or public}}
fileprivate func fileprivateFunction() {}
// expected-note@-1{{global function 'fileprivateFunction()' is not '@usableFromInline' or public}}
func internalFunction() {}
// expected-note@-1{{global function 'internalFunction()' is not '@usableFromInline' or public}}
@usableFromInline func versionedFunction() {}
public func publicFunction() {}

@_alwaysEmitIntoClient public func alwaysEmitIntoClientFunction() {
  privateFunction() // expected-error {{global function 'privateFunction()' is private and cannot be referenced from an '@_alwaysEmitIntoClient' function}}
  fileprivateFunction() // expected-error {{global function 'fileprivateFunction()' is fileprivate and cannot be referenced from an '@_alwaysEmitIntoClient' function}}
  internalFunction() // expected-error {{global function 'internalFunction()' is internal and cannot be referenced from an '@_alwaysEmitIntoClient' function}}
  versionedFunction()
  publicFunction()
}

public struct TestInitAccessors {
  var _x: Int

  public var x: Int {
    @storageRestrictions(initializes: _x)
    init { // expected-note 2 {{init accessor for property 'x' is not '@usableFromInline' or public}}
      self._x = newValue
    }

    get {
      self._x
    }

     set {}
   }

   @_alwaysEmitIntoClient
   public init(x: Int) {
     self.x = 0 // expected-error {{init accessor for property 'x' is internal and cannot be referenced from an '@_alwaysEmitIntoClient' function}}
   }

   @inlinable
   public init() {
     self.x = 0 // expected-error {{init accessor for property 'x' is internal and cannot be referenced from an '@inlinable' function}}
   }
}
