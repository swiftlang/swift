// RUN: %target-typecheck-verify-swift

// The bare @_preInverseGenerics does NOT require the experimental feature.
@_preInverseGenerics
func bare<T: ~Copyable>(_ t: borrowing T) {}

// The 'except:' form DOES require the experimental feature.
@_preInverseGenerics(except: ~Copyable) // expected-error {{'@_preInverseGenerics' is an experimental feature; use '-enable-experimental-feature PreInverseGenericsExcept'}}
func exceptCopyable<T: ~Copyable & ~Escapable>(_ t: borrowing T) {}

// An invalid attribute with 'except:' still DOES require the feature.
@_preInverseGenerics(except: Int) // expected-error {{'@_preInverseGenerics' is an experimental feature; use '-enable-experimental-feature PreInverseGenericsExcept'}}
                                  // expected-error@-1 {{'except' argument to '@_preInverseGenerics' must consist only of inverse constraints such as '~Copyable' or '~Escapable'}}
func exceptInt<T: ~Copyable & ~Escapable>(_ t: borrowing T) {}


