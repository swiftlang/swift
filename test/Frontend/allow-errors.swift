// RUN: %empty-directory(%t)

// The module should be generated regardless of errors and diagnostic should still be output
// RUN: %target-swift-frontend -verify -emit-module -o %t/errors.swiftmodule -experimental-allow-module-with-compiler-errors %s
// RUN: llvm-bcanalyzer %t/errors.swiftmodule | %FileCheck -check-prefix=CHECK-BC %s
// CHECK-BC-NOT: UnknownCode

struct InvalidStruct {
  let memberMissingType: undefined // expected-error {{cannot find type 'undefined'}}
}

func invalidFunc() -> InvalidStruct {
  ret // expected-error {{cannot find 'ret'}}
}
