// Matching rules: which imported C++ declaration (if any) a
// `@cxx @implementation` global function implements, and which matches are
// rejected.

// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-objc-interop \
// RUN:   -I %S/Inputs

// REQUIRES: swift_feature_CxxImplementation

import Matching


// Name

// expected-error@+1{{could not find imported function 'missingDeclaration' matching global function 'missingDeclaration'; make sure you import the module or header that declares it}}
@cxx @implementation
func missingDeclaration(_: Int32) -> Int32 {
  return 0
}

@cxx @implementation
func existingDeclaration1(_: Int32) -> Int32 {
  return 0
}

// expected-error@+1{{could not find imported function 'missingDeclaration2' matching global function 'existingDeclaration2'; make sure you import the module or header that declares it}}
@cxx(missingDeclaration2) @implementation
func existingDeclaration2(_: Int32) -> Int32 {
  return 0
}

@cxx(existingDeclaration3) @implementation
func cxxExistingDeclaration3(_: Int32) -> Int32 {
  return 0
}


// Duplicates

// expected-note@+2{{previously implemented here}}
@cxx @implementation
func dupFunc(_: Int32) -> Int32 {
  return 0
}

// expected-error@+1{{duplicate implementation of imported global function 'dupFunc'}}
@cxx(dupFunc) @implementation
func dupFuncAlias(_: Int32) -> Int32 {
  return 0
}


// Runtime clobbers

// expected-warning@+2{{symbol name 'swift_retain' is reserved for the Swift runtime}}
@cxx @implementation
func swift_retain(_: Int32) -> Int32 {
  return 0
}

// ok -- we emit mangled symbol (e.g., _Z13swift_releasei)
@cxx @implementation
func swift_release(_: Int32) -> Int32 {
  return 0
}

// expected-warning@+2{{symbol name 'swift_retain' is reserved for the Swift runtime}}
@cxx @implementation
func funcWithSwiftRetainAsmLabel(_: Int32) -> Int32 {
  return 0
}


// Inline

// expected-error@+2{{global function 'inlineDefinition' cannot implement C++ function 'inlineDefinition' because it already has a definition}}
@cxx @implementation
func inlineDefinition(_: Int32) -> Int32 {
  return 0
}

// expected-error@+2{{global function 'inlineDeclaration' cannot implement C++ function 'inlineDeclaration' because it is declared 'inline'}}
@cxx @implementation
func inlineDeclaration(_: Int32) -> Int32 {
  return 0
}

// expected-error@+2{{global function 'constexprFunc' cannot implement C++ function 'constexprFunc' because it is 'constexpr' (implicitly 'inline')}}
@cxx @implementation
func constexprFunc(_: Int32) -> Int32 {
  return 0
}


// Internal linkage (static)

// expected-error@+1{{could not find imported function 'staticFunc' matching global function 'staticFunc'; make sure you import the module or header that declares it}}
@cxx @implementation
func staticFunc(_: Int32) -> Int32 {
  return 0
}


// Type mismatch

// expected-error@+2{{global function 'typeMismatchParam' of type '(Float) -> Int32' does not match type '(CInt) -> CInt' (aka '(Int32) -> Int32') declared by the header}}
@cxx @implementation
func typeMismatchParam(_: Float) -> Int32 {
  return 0
}

// expected-error@+2{{global function 'typeMismatchReturn' of type '(Int32) -> Float' does not match type '(CInt) -> CInt' (aka '(Int32) -> Int32') declared by the header}}
@cxx @implementation
func typeMismatchReturn(_: Int32) -> Float {
  return 0
}

// expected-error@+2{{global function 'cxxTypeMismatchParamExplicitName' of type '(Float) -> Int32' does not match type '(CInt) -> CInt' (aka '(Int32) -> Int32') declared by the header}}
@cxx(typeMismatchParamExplicitName) @implementation
func cxxTypeMismatchParamExplicitName(_: Float) -> Int32 {
  return 0
}

// expected-error@+2{{global function 'cxxTypeMismatchReturnExplicitName' of type '(Int32) -> Float' does not match type '(CInt) -> CInt' (aka '(Int32) -> Int32') declared by the header}}
@cxx(typeMismatchReturnExplicitName) @implementation
func cxxTypeMismatchReturnExplicitName(_: Int32) -> Float {
  return 0
}

// ok -- typeMismatchRenamed wouldn't match, but we are targeting
// typeMatchRenamed.
@cxx(typeMatchRenamed) @implementation
func typeMismatchRenamed(_: Float) -> Int32 {
  return 0
}


// Templates

// expected-error@+1{{could not find imported function 'templateFunc' matching global function 'templateFunc'; make sure you import the module or header that declares it}}
@cxx @implementation
func templateFunc(_: Int32) -> Int32 {
  return 0
}


// Variadic args

// expected-error@+1{{could not find imported function 'variadicFunc' matching global function 'variadicFunc'; make sure you import the module or header that declares it}}
@cxx @implementation
func variadicFunc(_: Int32) -> Int32 {
  return 0
}


// Overloads
// TODO: This should work.

// expected-error@+1{{could not find imported function 'sameArityOverload' matching global function 'sameArityOverload'; make sure you import the module or header that declares it}}
@cxx @implementation
func sameArityOverload(_: Int32) -> Int32 {
  return 0
}


// C++ references
// TODO: The examples below actually do match, but not necessarily with the
// types we want. They should use unsafe pointers instead.

// expected-error@+2{{global function 'takesConstRef' cannot implement C++ function 'takesConstRef' because reference parameters and return types are not yet supported}}
@cxx @implementation
func takesConstRef(_: Int32) -> Int32 {
  return 0
}

// expected-error@+2{{global function 'takesMutableRef' cannot implement C++ function 'takesMutableRef' because reference parameters and return types are not yet supported}}
@cxx @implementation
func takesMutableRef(_: inout Int32) {
}

// expected-error@+2{{global function 'returnsMutableRef()' cannot implement C++ function 'returnsMutableRef' because reference parameters and return types are not yet supported}}
@cxx @implementation
func returnsMutableRef() -> UnsafeMutablePointer<Int32> {
  fatalError()
}
