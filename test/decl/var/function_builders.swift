// RUN: %target-typecheck-verify-swift

@functionBuilder // expected-error {{'@functionBuilder' attribute cannot be applied to this declaration}}
var globalBuilder: Int

@functionBuilder // expected-error {{'@functionBuilder' attribute cannot be applied to this declaration}}
func globalBuilderFunction() -> Int { return 0 }

@functionBuilder
struct Maker {}

@functionBuilder
class Inventor {}

@Maker // expected-error {{function builder attribute 'Maker' can only be applied to a parameter}}
var global: Int

@Maker // expected-error {{function builder attribute 'Maker' can only be applied to a parameter}}
func globalFunction() {}

@Maker // expected-error {{function builder attribute 'Maker' can only be applied to a parameter}}
func globalFunctionWithFunctionParam(fn: () -> ()) {}

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

@functionBuilder
struct GenericMaker<T> {} // expected-note {{generic type 'GenericMaker' declared here}}

struct GenericContainer<T> {  // expected-note {{generic type 'GenericContainer' declared here}}
  @functionBuilder
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


struct WithinGeneric<U> {
  func makeParamBoundInContext(@GenericMaker<U>  // expected-error {{function builder type 'GenericMaker<U>' cannot depend on the current generic context}}
                               fn: () -> ()) {}
}
