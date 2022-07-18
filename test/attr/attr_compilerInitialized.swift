// RUN: %target-typecheck-verify-swift

@propertyWrapper
struct Pushin {
  var wrappedValue: String

  init(_ thing: String) {
    wrappedValue = thing
  }
}

protocol P {
  // expected-error@+1 {{'@_compilerInitialized' is not currently supported in protocols}}
  @_compilerInitialized var bad_requirement: Int { get }
}

class Facts {
  // expected-error@+1 {{'@_compilerInitialized' attribute only applies to let-bound stored properties without a default value}}
  @_compilerInitialized var bad_mutable: Int

  // expected-error@+1 {{'@_compilerInitialized' attribute only applies to let-bound stored properties without a default value}}
  @_compilerInitialized var bad_hasObserver: Int {
    didSet {}
  }

  // expected-error@+1 {{'@_compilerInitialized' attribute only applies to let-bound stored properties without a default value}}
  @_compilerInitialized let bad_hasInitialValue = 0

  // expected-error@+1 {{'@_compilerInitialized' attribute only applies to let-bound stored properties without a default value}}
  @_compilerInitialized @Pushin("🅿️") var bad_hasWrapper

  // expected-error@+1 {{'@_compilerInitialized' cannot be applied to an Optional let}}
  @_compilerInitialized let bad_optional: Int?

  // expected-error@+1 {{'@_compilerInitialized' attribute only applies to let-bound stored properties without a default value}}
  @_compilerInitialized static let bad_static1: String = ""

  // expected-note@+3 {{add an initializer to silence this error}}
  // expected-error@+2 {{'static let' declaration requires an initializer expression or an explicitly stated getter}}
  // expected-error@+1 {{'@_compilerInitialized' can only be applied to a non-static class or actor member}}
  @_compilerInitialized static let bad_static2: String

  @_compilerInitialized let id: Int

  init(){}
}

struct Logic {
  // expected-error@+1 {{'@_compilerInitialized' can only be applied to a non-static class or actor member}}
  @_compilerInitialized let bad_structmember: String
}

// expected-error@+1 {{'@_compilerInitialized' can only be applied to a non-static class or actor member}}
@_compilerInitialized let bestID: String


