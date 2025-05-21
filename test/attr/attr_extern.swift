// RUN: %target-typecheck-verify-swift -enable-experimental-feature Extern -disable-availability-checking

// https://github.com/apple/swift/issues/70776
// REQUIRES: github70776
// REQUIRES: swift_feature_Extern

@_extern(wasm, module: "m1", name: "f1")
func f1(x: Int) -> Int

@_extern(wasm, module: "m2", name: ) // expected-error  {{expected string literal in '_extern' attribute}}
func f2ErrorOnMissingNameLiteral(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@_extern(wasm, module: "m3", name) // expected-error  {{expected ':' after label 'name'}}
func f3ErrorOnMissingNameColon(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@_extern(wasm, module: "m4",) // expected-error  {{expected name argument to @_extern attribute}}
func f4ErrorOnMissingNameLabel(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@_extern(wasm, module: "m5") // expected-error {{expected ',' in '_extern' attribute}}
func f5ErrorOnMissingName(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@_extern(wasm, module: ) // expected-error {{expected string literal in '_extern' attribute}} expected-error {{expected ',' in '_extern' attribute}}
func f6ErrorOnMissingModuleLiteral(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@_extern(wasm, module) // expected-error {{expected ':' after label 'module'}} expected-error {{expected ',' in '_extern' attribute}}
func f7ErrorOnMissingModuleColon(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@_extern(wasm,) // expected-error {{expected module argument to @_extern attribute}} expected-error {{expected ',' in '_extern' attribute}}
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

@_extern(invalid, module: "m12", name: "f12") // expected-error {{expected '_extern' option such as 'c'}}
func f12InvalidLang() // expected-error {{expected '{' in body of function declaration}}

@_extern(c, "valid")
func externCValid()

@_extern(c, "_start_with_underscore")
func underscoredValid()

@_extern(c, "") // expected-error {{expected non-empty C name in @_extern attribute}}
func emptyCName()

// Allow specifying any identifier explicitly
@_extern(c, "0start_with_digit")
func explicitDigitPrefixed()

@_extern(c) // expected-warning {{C name '+' may be invalid; explicitly specify the name in '@_extern(c)' to suppress this warning}}
func +(a: Int, b: Bool) -> Bool

@_extern(c) // expected-warning {{C name '🥸_implicitInvalid' may be invalid; explicitly specify the name in '@_extern(c)' to suppress this warning}}
func 🥸_implicitInvalid()

@_extern(c)
func omitCName()

@_extern(c, ) // expected-error {{expected string literal in '_extern' attribute}}
func editingCName() // expected-error {{expected '{' in body of function declaration}}

struct StructScopeC {
    @_extern(c, "member_decl") // expected-error {{@_extern attribute can only be applied to global functions}}
    func memberDecl()

    @_extern(c, "static_member_decl")
    static func staticMemberDecl()
}

func funcScopeC() {
    @_extern(c, "func_scope_inner")
    func inner()
}

@_extern(c, "c_value") // expected-error {{@_extern may only be used on 'func' declarations}}
var nonFunc: Int = 0

@_extern(c, "with_body")
func withInvalidBody() {} // expected-error {{unexpected body of function declaration}}

@_extern(c, "duplicate_attr_c_1")
@_extern(c, "duplicate_attr_c_2") // expected-error {{duplicate attribute}}
func duplicateAttrsC()

@_extern(wasm, module: "dup", name: "duplicate_attr_wasm_1")
@_extern(wasm, module: "dup", name: "duplicate_attr_wasm_2") // expected-error {{duplicate attribute}}
func duplicateAttrsWasm()

@_extern(c, "mixed_attr_c")
@_extern(wasm, module: "mixed", name: "mixed_attr_wasm")
func mixedAttrs_C_Wasm()

class NonC {}
@_extern(c)
func nonCReturnTypes() -> NonC // expected-error {{'NonC' cannot be represented in C}}
// @_extern(wasm) have no interface limitation
@_extern(wasm, module: "non-c", name: "return_wasm")
func nonCReturnTypesWasm() -> NonC
@_extern(c)
@_extern(wasm, module: "non-c", name: "return_mixed")
func nonCReturnTypesMixed() -> NonC // expected-error {{'NonC' cannot be represented in C}}

@_extern(c)
func nonCParamTypes(_: Int, _: NonC) // expected-error {{'NonC' cannot be represented in C}}
@_extern(wasm, module: "non-c", name: "param_wasm")
func nonCParamTypesWasm(_: Int, _: NonC)

@_extern(c)
@_extern(wasm, module: "non-c", name: "param_mixed")
func nonCParamTypesMixed(_: Int, _: NonC) // expected-error {{'NonC' cannot be represented in C}}

@_extern(c)
func defaultArgValue_C(_: Int = 42)

@_extern(wasm, module: "", name: "")
func defaultArgValue_Wasm(_: Int = 24)

@_extern(c)
func asyncFuncC() async // expected-error {{async functions cannot be represented in C}}

@_extern(c)
func throwsFuncC() throws // expected-error {{raising errors from C functions is not supported}}

@_extern(c)
func genericFuncC<T>(_: T) // expected-error {{'T' cannot be represented in C}}

@_extern(c) // expected-error {{@_extern attribute cannot be applied to an '@_cdecl' declaration}}
@_cdecl("another_c_name")
func withAtCDecl_C()

@_extern(wasm, module: "", name: "") // expected-error {{@_extern attribute cannot be applied to an '@_cdecl' declaration}}
@_cdecl("another_c_name")
func withAtCDecl_Wasm()

@_extern(c) // expected-error {{@_extern attribute cannot be applied to an '@_silgen_name' declaration}}
@_silgen_name("another_sil_name")
func withAtSILGenName_C()

@_extern(wasm, module: "", name: "") // expected-error {{@_extern attribute cannot be applied to an '@_silgen_name' declaration}}
@_silgen_name("another_sil_name")
func withAtSILGenName_Wasm()

@_extern(c) // expected-error {{@_extern attribute cannot be applied to an '@_silgen_name' declaration}} expected-error {{@_extern attribute cannot be applied to an '@_cdecl' declaration}}
@_cdecl("another_c_name")
@_silgen_name("another_sil_name")
func withAtSILGenName_CDecl_C()
