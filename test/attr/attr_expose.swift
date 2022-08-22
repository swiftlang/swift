// RUN: %target-typecheck-verify-swift

@_cdecl("cdecl_foo") func foo(x: Int) -> Int { return x }

@_expose(Cxx) func test() {}
@_expose(Cxx, "renamed") func renamedFunc() {}
@_expose(Cxx, "") func emptyNameOk() {}

@_expose // expected-error {{expected '(' in '_expose' attribute}}
func exposeWithoutArgs() {}

@_expose(CplusPlus) // expected-error {{expected '_expose' option such as 'Cxx'}}
func incorrectLangSpecifier() {}

@_expose(Cxx) @_cdecl("test") // expected-error {{@_expose attribute cannot be applied to an '@_cdecl' declaration}}
func cdeclAndExpose() {}

func hasNested() {
  @_expose(Cxx) // expected-error{{can only be used in a non-local scope}}
  func nested() { }
}

struct NotExposedStruct {
    @_expose(Cxx) // expected-error {{@_expose attribute cannot be applied inside of unexposed declaration 'NotExposedStruct'}}
    func errorOnInnerExpose() {}
}
