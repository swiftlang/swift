// RUN: %target-typecheck-verify-swift

@_functionBuilder // expected-error {{'@_functionBuilder' attribute cannot be applied to this declaration}}
var globalBuilder: Int

@_functionBuilder // expected-error {{'@_functionBuilder' attribute cannot be applied to this declaration}}
func globalBuilderFunction() -> Int { return 0 }

@_functionBuilder
struct Maker {}

@_functionBuilder
class Inventor {}

@Maker // expected-error {{function builder attribute 'Maker' can only be applied to a parameter, function, or computed property}}
typealias typename = Inventor

@Maker // expected-error {{function builder attribute 'Maker' can only be applied to a variable if it defines a getter}}
var global: Int

// FIXME: should this be allowed?
@Maker
var globalWithEmptyImplicitGetter: Int {}
// expected-error@-1 {{computed property must have accessors specified}}
// expected-error@-3 {{function builder attribute 'Maker' can only be applied to a variable if it defines a getter}}

@Maker
var globalWithEmptyExplicitGetter: Int { get {} }  // expected-error{{type 'Maker' has no member 'buildBlock'}}

@Maker
var globalWithSingleGetter: Int { 0 } // expected-error {{ype 'Maker' has no member 'buildBlock'}}

@Maker
var globalWithMultiGetter: Int { 0; 0 } // expected-error {{ype 'Maker' has no member 'buildBlock'}}

@Maker
func globalFunction() {} // expected-error {{ype 'Maker' has no member 'buildBlock'}}

@Maker
func globalFunctionWithFunctionParam(fn: () -> ()) {}  // expected-error {{ype 'Maker' has no member 'buildBlock'}}

func makerParam(@Maker
                fn: () -> ()) {}

// FIXME: these diagnostics are reversed?
func makerParamRedundant(@Maker // expected-error {{only one function builder attribute can be attached to a parameter}}
                         @Maker // expected-note {{previous function builder specified here}}
                         fn: () -> ()) {}

func makerParamConflict(@Maker // expected-error {{only one function builder attribute can be attached to a parameter}}
                        @Inventor // expected-note {{previous function builder specified here}}
                        fn: () -> ()) {}

func makerParamMissing1(@Missing // expected-error {{unknown attribute 'Missing'}}
                        @Maker
                        fn: () -> ()) {}

func makerParamMissing2(@Maker
                        @Missing // expected-error {{unknown attribute 'Missing'}}
                        fn: () -> ()) {}

func makerParamExtra(@Maker(5) // expected-error {{function builder attributes cannot have arguments}}
                     fn: () -> ()) {}

func makerParamAutoclosure(@Maker // expected-error {{function builder attribute 'Maker' cannot be applied to an autoclosure parameter}}
                           fn: @autoclosure () -> ()) {}

@_functionBuilder
struct GenericMaker<T> {} // expected-note {{generic type 'GenericMaker' declared here}}

struct GenericContainer<T> {  // expected-note {{generic type 'GenericContainer' declared here}}
  @_functionBuilder
  struct Maker {}
}

func makeParamUnbound(@GenericMaker // expected-error {{reference to generic type 'GenericMaker' requires arguments}}
                      fn: () -> ()) {}

func makeParamBound(@GenericMaker<Int>
                    fn: () -> ()) {}

func makeParamNestedUnbound(@GenericContainer.Maker // expected-error {{reference to generic type 'GenericContainer' requires arguments}}
                            fn: () -> ()) {}

func makeParamNestedBound(@GenericContainer<Int>.Maker
                          fn: () -> ()) {}


protocol P { }

@_functionBuilder
struct ConstrainedGenericMaker<T: P> {}


struct WithinGeneric<U> {
  func makeParamBoundInContext(@GenericMaker<U> fn: () -> ()) {}

  // expected-error@+1{{type 'U' does not conform to protocol 'P'}}
  func makeParamBoundInContextBad(@ConstrainedGenericMaker<U>
    fn: () -> ()) {}
}
