// RUN: %target-typecheck-verify-swift -disable-availability-checking -I %S/Inputs/has_symbol/

// UNSUPPORTED: OS=windows-msvc

@_weakLinked import has_symbol_helper_clang

func testFunctions() {
  if #_hasSymbol(clangFunc) {}
  if #_hasSymbol(clangFunc(_:)) {}
}

func testGlobalVars() {
  // FIXME: Add support for clang global vars
  if #_hasSymbol(clangGlobalVar) {} // expected-error {{'#_hasSymbol' cannot be used with this declaration}}
}

func testTypes() {
  if #_hasSymbol(ClangStruct.self) {} // expected-error {{'#_hasSymbol' cannot be used with this declaration}}
  if #_hasSymbol(ClangEnum.self) {} // expected-error {{'#_hasSymbol' cannot be used with this declaration}}
}

func testMacros() {
  if #_hasSymbol(CONSTANT_MACRO) {} // FIXME: This should be diagnosed
}

func testEnum() {
  if #_hasSymbol(ClangEnumMember) {} // expected-error {{'#_hasSymbol' cannot be used with this declaration}}
}

func testStruct(_ s: ClangStruct) {
  if #_hasSymbol(s.x) {} // expected-error {{'#_hasSymbol' cannot be used with this declaration}}
}
