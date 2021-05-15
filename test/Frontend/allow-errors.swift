// RUN: %empty-directory(%t)

// The module should be generated regardless of errors, including .swiftdeps, .d,
// .swiftsourceinfo, etc. Diagnostics should still be output as well

// RUN: %target-swift-frontend -verify -emit-module -o %t/errors.swiftmodule -emit-module-source-info -emit-module-doc -emit-reference-dependencies-path %t/errors.swiftdeps -emit-dependencies-path %t/errors.d -experimental-allow-module-with-compiler-errors -primary-file %s
// RUN: llvm-bcanalyzer %t/errors.swiftmodule | %FileCheck -check-prefix=CHECK-BC %s
// RUN: ls %t/errors.swiftdeps %t/errors.d %t/errors.swiftsourceinfo %t/errors.swiftdoc
// CHECK-BC-NOT: UnknownCode

public func invalid() -> undefined {} // expected-error {{cannot find type 'undefined'}}
