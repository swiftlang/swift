// RUN: %target-typecheck-verify-swift -enable-experimental-feature Extern -disable-availability-checking

@extern(wasm, module: "m1", name: "f1")
func f1(x: Int) -> Int

@extern(wasm, module: "m2", name: ) // expected-error  {{expected string literal in 'extern' attribute}}
func f2ErrorOnMissingNameLiteral(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@extern(wasm, module: "m3", name) // expected-error  {{expected ':' after label 'name'}}
func f3ErrorOnMissingNameColon(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@extern(wasm, module: "m4",) // expected-error  {{expected name argument to @extern attribute}}
func f4ErrorOnMissingNameLabel(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@extern(wasm, module: "m5") // expected-error {{expected ',' in 'extern' attribute}}
func f5ErrorOnMissingName(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@extern(wasm, module: ) // expected-error {{expected string literal in 'extern' attribute}} expected-error {{expected ',' in 'extern' attribute}}
func f6ErrorOnMissingModuleLiteral(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@extern(wasm, module) // expected-error {{expected ':' after label 'module'}} expected-error {{expected ',' in 'extern' attribute}}
func f7ErrorOnMissingModuleColon(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@extern(wasm,) // expected-error {{expected module argument to @extern attribute}} expected-error {{expected ',' in 'extern' attribute}}
func f8ErrorOnMissingModuleLabel(x: Int) -> Int // expected-error{{expected '{' in body of function declaration}}

@extern(wasm, module: "m9", name: "f9")
func f9WithBody() {} // expected-error {{unexpected body of function declaration}}

struct S {
    @extern(wasm, module: "m10", name: "f10") // expected-error {{@extern attribute can only be applied to global functions}}
    func f10Member()
}

func f11Scope() {
    @extern(wasm, module: "m11", name: "f11")
    func f11Inner()
}

@extern(invalid, module: "m12", name: "f12") // expected-error {{expected 'extern' option such as 'c'}}
func f12InvalidLang() // expected-error {{expected '{' in body of function declaration}}

@extern(c, "valid")
func externCValid()

@extern(c, "_start_with_underscore")
func underscoredValid()

@extern(c, "") // expected-error {{expected non-empty C name in @extern attribute}}
func emptyCName()

// Allow specifying any identifier explicitly
@extern(c, "0start_with_digit")
func explicitDigitPrefixed()

@extern(c) // expected-warning {{C name '+' may be invalid; explicitly specify the name in @extern(c) to suppress this warning}}
func +(a: Int, b: Bool) -> Bool

@extern(c) // expected-warning {{C name '🥸_implicitInvalid' may be invalid; explicitly specify the name in @extern(c) to suppress this warning}}
func 🥸_implicitInvalid()

@extern(c)
func omitCName()

@extern(c, ) // expected-error {{expected string literal in 'extern' attribute}}
func editingCName() // expected-error {{expected '{' in body of function declaration}}

struct StructScopeC {
    @extern(c, "member_decl") // expected-error {{@extern attribute can only be applied to global functions}}
    func memberDecl()

    @extern(c, "static_member_decl")
    static func staticMemberDecl()
}

func funcScopeC() {
    @extern(c, "func_scope_inner")
    func inner()
}

@extern(c, "c_value") // expected-error {{@extern may only be used on 'func' declarations}}
var nonFunc: Int = 0

@extern(c, "with_body")
func withInvalidBody() {} // expected-error {{unexpected body of function declaration}}

@extern(c, "duplicate_attr_c_1")
@extern(c, "duplicate_attr_c_2") // expected-error {{duplicate attribute}}
func duplicateAttrsC()

@extern(wasm, module: "dup", name: "duplicate_attr_wasm_1")
@extern(wasm, module: "dup", name: "duplicate_attr_wasm_2") // expected-error {{duplicate attribute}}
func duplicateAttrsWasm()

@extern(c, "mixed_attr_c")
@extern(wasm, module: "mixed", name: "mixed_attr_wasm")
func mixedAttrs_C_Wasm()

class NonC {}
@extern(c)
func nonCReturnTypes() -> NonC // expected-error {{'NonC' cannot be represented in C}}
// @extern(wasm) have no interface limitation
@extern(wasm, module: "non-c", name: "return_wasm")
func nonCReturnTypesWasm() -> NonC
@extern(c)
@extern(wasm, module: "non-c", name: "return_mixed")
func nonCReturnTypesMixed() -> NonC // expected-error {{'NonC' cannot be represented in C}}

@extern(c)
func nonCParamTypes(_: Int, _: NonC) // expected-error {{'NonC' cannot be represented in C}}
@extern(wasm, module: "non-c", name: "param_wasm")
func nonCParamTypesWasm(_: Int, _: NonC)

@extern(c)
@extern(wasm, module: "non-c", name: "param_mixed")
func nonCParamTypesMixed(_: Int, _: NonC) // expected-error {{'NonC' cannot be represented in C}}

@extern(c)
func defaultArgValue_C(_: Int = 42)

@extern(wasm, module: "", name: "")
func defaultArgValue_Wasm(_: Int = 24)

@extern(c)
func asyncFuncC() async // expected-error {{async functions cannot be represented in C}}

@extern(c)
func throwsFuncC() throws // expected-error {{raising errors from C functions is not supported}}

@extern(c)
func genericFuncC<T>(_: T) // expected-error {{'T' cannot be represented in C}}

@extern(c) // expected-error {{@extern attribute cannot be applied to an '@_cdecl' declaration}}
@_cdecl("another_c_name")
func withAtCDecl_C()

@extern(wasm, module: "", name: "") // expected-error {{@extern attribute cannot be applied to an '@_cdecl' declaration}}
@_cdecl("another_c_name")
func withAtCDecl_Wasm()

@extern(c) // expected-error {{@extern attribute cannot be applied to an '@_silgen_name' declaration}}
@_silgen_name("another_sil_name")
func withAtSILGenName_C()

@extern(wasm, module: "", name: "") // expected-error {{@extern attribute cannot be applied to an '@_silgen_name' declaration}}
@_silgen_name("another_sil_name")
func withAtSILGenName_Wasm()

@extern(c) // expected-error {{@extern attribute cannot be applied to an '@_silgen_name' declaration}} expected-error {{@extern attribute cannot be applied to an '@_cdecl' declaration}}
@_cdecl("another_c_name")
@_silgen_name("another_sil_name")
func withAtSILGenName_CDecl_C()
