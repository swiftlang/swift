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