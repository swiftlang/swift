// RUN: %target-typecheck-verify-swift

@_extern(wasm, module: "m1", name: "f1")
func f1(x: Int) -> Int

@_extern(wasm, module: "m2", name: ) // expected-error  {{expected string literal in '_extern' attribute}}
func f2ErrorOnMissingNameLiteral(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@_extern(wasm, module: "m3", name) // expected-error  {{expected ':' after label 'name'}}
func f3ErrorOnMissingNameColon(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@_extern(wasm, module: "m4",) // expected-error  {{expected name argument to @_extern attribute}}
func f4ErrorOnMissingNameLabel(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@_extern(wasm, module: "m5") // expected-error {{expected name argument to @_extern attribute}}
func f5ErrorOnMissingName(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@_extern(wasm, module: ) // expected-error {{expected string literal in '_extern' attribute}} expected-error {{expected name argument to @_extern attribute}}
func f6ErrorOnMissingModuleLiteral(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@_extern(wasm, module) // expected-error {{expected ':' after label 'module'}} expected-error {{expected name argument to @_extern attribute}}
func f7ErrorOnMissingModuleColon(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@_extern(wasm,) // expected-error {{expected module argument to @_extern attribute}} expected-error {{expected name argument to @_extern attribute}}
func f8ErrorOnMissingModuleLabel(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@_extern(wasm, module: "m9", name: "f9")
func f9WithBody() {} // expected-error {{unexpected body of function declaration}}

struct S {
    @_extern(wasm, module: "m10", name: "f10") // expected-error {{@_extern attribute can only be applied to global functions}}
    func f10Member()
}

func f11Scope() {
    @_extern(wasm, module: "m11", name: "f11")
    func f11Inner()
}

@_extern(invalid, module: "m12", name: "f12") // expected-error {{expected '_extern' option such as 'wasm'}}
func f12InvalidLang() // expected-error {{expected '{' in body of function declaration}}
