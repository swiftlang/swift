// RUN: %target-typecheck-verify-swift -verify-additional-prefix no-feature-
// RUN: %target-typecheck-verify-swift -enable-experimental-feature AnyReference

// REQUIRES: swift_feature_AnyReference

func foo<T: _AnyReference>(_: T) {}
// expected-no-feature-error@-1 {{using '_AnyReference' requires an experimental feature; use '-enable-experimental-feature AnyReference'}}

typealias Alias = _AnyReference
// expected-no-feature-error@-1 {{using '_AnyReference' requires an experimental feature; use '-enable-experimental-feature AnyReference'}} 

class C : _AnyReference {}
// expected-no-feature-error@-1 {{using '_AnyReference' requires an experimental feature; use '-enable-experimental-feature AnyReference'}} 

class D<T> where T : _AnyReference {} 
// expected-no-feature-error@-1 {{using '_AnyReference' requires an experimental feature; use '-enable-experimental-feature AnyReference'}} 

protocol P {}

func foo<T: _AnyReference & P>(_: T) {} 
// expected-no-feature-error@-1 {{using '_AnyReference' requires an experimental feature; use '-enable-experimental-feature AnyReference'}} 

// expected-no-feature-error@+1 {{using '_AnyReference' requires an experimental feature; use '-enable-experimental-feature AnyReference'}}
@_specialize(where T: _AnyReference)
func specializedType<T>(_: T) {}
