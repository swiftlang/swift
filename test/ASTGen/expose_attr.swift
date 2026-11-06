// '@_expose' option parsing, and attributes that are only valid outside local
// scopes, checked against both parsers.

// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

@_expose(Cxx) func exposed() {}
@_expose(wasm) func exposedToWasm() {}

@_expose(CplusPlus) // expected-error {{expected '_expose' option such as 'Cxx'}}
func incorrectLangSpecifier() {}

@_expose(!Cxx, "name") // expected-error {{'@_expose(!Cxx)' does not accept a name argument}}
func notExposeWithName() {}

@_expose(Cxx)
@_expose(Cxx) // expected-error {{duplicate attribute}}
func exposeTwice() {}

func hasNested() {
  @_expose(Cxx) // expected-error {{'@_expose' can only be used in a non-local scope}}
  func exposedNested() {}

  @_cdecl("nested") // expected-error {{'@_cdecl' can only be used in a non-local scope}}
  func cdeclNested() {}
}
