// RUN: %target-typecheck-verify-swift

class ThrowingInitSuperclass {
  init() throws { }
}

// Implicitly calling the super.init IS possible here because the initializer
// is a throwing initializer.
class ThrowingInitClass: ThrowingInitSuperclass {
  init(simpleArgument: Int) throws { }
}

// Implicitly calling the super.init IS NOT possible here because the
// initializer is not a throwing initializer.
class NonThrowingInitClass: ThrowingInitSuperclass {
  init(simpleArgument: Int) { } // expected-error {{missing call to superclass's initializer; 'super.init' is a throwing initializer and requires either an explicit call or that this initializer is also marked as 'throws'}}
}

// Implicitly calling the super.init IS NOT possible here because the
// initializer is a rethrowing initializer, which means it can only
// rethrow errors from its parameters.
class RethrowingInitClass: ThrowingInitSuperclass {
  init(throwingArgument: () throws -> Void) rethrows { } // expected-error {{missing call to superclass's initializer; 'super.init' is a throwing initializer and requires either an explicit call or that this initializer is also marked as 'throws'}}
}
