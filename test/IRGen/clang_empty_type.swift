// RUN: %target-swift-frontend -emit-ir -verify -enable-objc-interop -import-objc-header %S/Inputs/clang_empty_type.h %s

public func projectTrailingArray(x: inout TrailingArray) {
  x.size = 2
  x.buffer = ()
}
