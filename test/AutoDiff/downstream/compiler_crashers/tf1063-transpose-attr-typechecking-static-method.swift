// RUN: not --crash %target-swift-frontend-typecheck -primary-file %s
// REQUIRES: asserts

// TF-1063: `@transpose` attribute type-checking crash for static methods in
// `AnyFunctionType::getTransposeOriginalFunctionType`.

struct Struct: Differentiable & AdditiveArithmetic {
  static func staticMethod(x: Struct) -> Struct {
    x
  }

  @transpose(of: staticMethod, wrt: 0)
  static func transposeStaticMethod() -> Struct {
    self
  }
}

// Assertion failed: (!empty()), function back, file llvm-project/llvm/include/llvm/ADT/ArrayRef.h, line 158.
// Stack dump:
// ...
// 1.	Swift version 5.2-dev (Swift ace3925395)
// 2.	While evaluating request TypeCheckSourceFileRequest(source_file "test/AutoDiff/compiler_crashers/tf1063-transpose-attr-typechecking-static-method.swift", 0)
// 3.	While type-checking 'Struct' (at test/AutoDiff/compiler_crashers/tf1063-transpose-attr-typechecking-static-method.swift:6:1)
// 4.	While type-checking 'transposeStaticMethod()' (at test/AutoDiff/compiler_crashers/tf1063-transpose-attr-typechecking-static-method.swift:12:3)
// ...
// 7  swiftc                   0x00000001111003c3 swift::AnyFunctionType::getTransposeOriginalFunctionType(swift::IndexSubset*, bool) (.cold.3) + 35
// 8  swiftc                   0x000000010dbf68e0 swift::AnyFunctionType::getTransposeOriginalFunctionType(swift::IndexSubset*, bool) + 1904
// 9  swiftc
