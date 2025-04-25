// RUN: %target-typecheck-verify-swift

@_cdecl("cdecl_foo") func foo(x: Int) -> Int { return x }

@_expose(Cxx) func test() {}
@_expose(Cxx, "renamed") func renamedFunc() {}
@_expose(Cxx, "") func emptyNameOk() {}

@_expose // expected-error {{expected '(' in '_expose' attribute}}
func exposeWithoutArgs() {}

@_expose(CplusPlus) // expected-error {{expected '_expose' option such as 'Cxx'}}
func incorrectLangSpecifier() {}

@_expose(Cxx) @_cdecl("test") // expected-error {{'@_expose' cannot be applied to an '@_cdecl' declaration}}
func cdeclAndExpose() {}

func hasNested() {
  @_expose(Cxx) // expected-error{{can only be used in a non-local scope}}
  func nested() { }
}

struct NotExposedStruct {
    @_expose(Cxx) // expected-error {{'@_expose' cannot be applied inside of unexposed declaration 'NotExposedStruct'}}
    func errorOnInnerExpose() {}
}

@_expose(Cxx)
struct ExposedStruct {
    @_expose(Cxx, "create") // expected-error {{invalid declaration name 'create' specified in '@_expose'; exposed initializer name must start with 'init'}}
    init() {}

    @_expose(Cxx, "initWith")
    init(with y: Int) {}
}
@_expose(Cxx)
@_expose(Cxx) // expected-error {{duplicate attribute}}
func exposeToCxxCxx() {}

@_expose(wasm) func exposeToWasm() {}
@_expose(wasm, "with_name") func wasmName() {}
@_expose(wasm, "") func wasmEmptyNameOk() {}

@_expose(wasm) struct WasmErrorOnStruct { // expected-error {{'@_expose' with 'wasm' can only be applied to global functions}}
  @_expose(wasm) func errorOnMethod() {} // expected-error {{'@_expose' with 'wasm' can only be applied to global functions}}
}

func wasmNested() {
  @_expose(wasm) // expected-error {{attribute '_expose' can only be used in a non-local scope}}
  func errorOnInnerExpose() {}
}

@_expose(Cxx)
@_expose(wasm)
func exposeToCxxWasm() {}

@_expose(wasm)
@_expose(Cxx)
func exposeToWasmCxx() {}

@_expose(wasm)
@_expose(wasm) // expected-error {{duplicate attribute}}
func exposeToWasmWasm() {}
