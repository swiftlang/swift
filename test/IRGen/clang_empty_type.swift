// RUN: %target-swift-frontend -emit-ir -verify -enable-objc-interop -import-objc-header %S/Inputs/clang_empty_type.h %s
// REQUIRES: objc_codegen

public func projectTrailingArray(x: inout TrailingArray) {
  x.size = 2
  x.buffer = ()
}
