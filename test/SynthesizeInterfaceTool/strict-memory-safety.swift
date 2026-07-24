// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-synthesize-interface -module-name CxxUnions -I %t/Inputs -cxx-interoperability-mode=default | %FileCheck %s

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
// CHECK:     @unsafe public var number: CInt
// CHECK:     @unsafe public var decimal: CFloat
