// RUN: %target-typecheck-verify-swift -enable-experimental-feature PreInverseGenericsExcept

// REQUIRES: swift_feature_PreInverseGenericsExcept

@_preInverseGenerics
func bare<T: ~Copyable>(_ t: borrowing T) {}

@_preInverseGenerics(except: ~Copyable)
func exceptCopyable<T: ~Copyable & ~Escapable>(_ t: borrowing T) {}

@_preInverseGenerics(except: ~Escapable)
func exceptEscapable<T: ~Copyable & ~Escapable>(_ t: borrowing T) {}

// excepting all inverses is equivalent to not using the attribute
@_preInverseGenerics(except: ~Copyable & ~Escapable) // expected-warning {{'@_preInverseGenerics' that excepts all inverse constraints is equivalent to not using the attribute}}
func exceptBoth<T: ~Copyable & ~Escapable>(_ t: borrowing T) {}

// `except: Any` is confusing; reject it in favor of the bare form.
@_preInverseGenerics(except: Any) // expected-error {{'except' argument to '@_preInverseGenerics' must consist only of inverse constraints such as '~Copyable' or '~Escapable'}}
func exceptAny<T: ~Copyable & ~Escapable>(_ t: borrowing T) {}

// wrong label
@_preInverseGenerics(foo: ~Copyable) // expected-error {{expected 'except:' argument in '@_preInverseGenerics'}}
func bad1() {}

// non-inverse type
@_preInverseGenerics(except: Int) // expected-error {{'except' argument to '@_preInverseGenerics' must consist only of inverse constraints such as '~Copyable' or '~Escapable'}}
func bad2() {}

// positive protocol (not inverted)
@_preInverseGenerics(except: Copyable) // expected-error {{'except' argument to '@_preInverseGenerics' must consist only of inverse constraints such as '~Copyable' or '~Escapable'}}
func bad3() {}

// Warning: attribute on extension has no effect
struct S<T: ~Copyable>: ~Copyable {}

@_preInverseGenerics // expected-warning {{'@_preInverseGenerics' has no effect on an extension; place it on individual members instead}}
extension S where T: ~Copyable {
  func extMethod() {}
}
