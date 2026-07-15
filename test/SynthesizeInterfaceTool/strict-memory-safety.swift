// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// By default, the `@unsafe` attribute that ClangImporter attaches is implicit
/// and hidden, so it is not printed.
// RUN: %target-swift-synthesize-interface -module-name CxxUnions -I %t/Inputs -cxx-interoperability-mode=default | %FileCheck %s --implicit-check-not=@unsafe

/// With -strict-memory-safety, unsafe declarations (e.g. C++ union field
/// accessors) are marked `@unsafe`.
// RUN: %target-swift-synthesize-interface -module-name CxxUnions -I %t/Inputs -cxx-interoperability-mode=default -strict-memory-safety | %FileCheck %s --check-prefix=UNSAFE

//--- Inputs/module.modulemap
module CxxUnions {
  requires cplusplus
  header "cxx-unions.h"
}

//--- Inputs/cxx-unions.h
#pragma once

union Value {
  int number;
  float decimal;
};

// CHECK: public struct Value {
// CHECK:     public var number: CInt
// CHECK:     public var decimal: CFloat

// UNSAFE: public struct Value {
// UNSAFE:     @unsafe public var number: CInt
// UNSAFE:     @unsafe public var decimal: CFloat
