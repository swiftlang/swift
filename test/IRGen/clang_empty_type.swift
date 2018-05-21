// RUN: %target-swift-frontend -emit-ir -verify -import-objc-header %S/Inputs/clang_empty_type.h %s

public func projectTrailingArray(x: inout TrailingArray) {
  x.size = 2
  x.buffer = ()
}
