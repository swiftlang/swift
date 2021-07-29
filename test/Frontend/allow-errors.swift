// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mods)
// RUN: touch %t/empty.swift

// The module should be generated regardless of errors, including .swiftdeps, .d,
// .swiftsourceinfo, etc. Diagnostics should still be output as well

// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/mods/errorsmain.partial.swiftmodule -emit-reference-dependencies-path %t/mods/errorsmain.partial.swiftdeps -experimental-allow-module-with-compiler-errors -primary-file %s
// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/mods/errorsempty.partial.swiftmodule %t/empty.swift
// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/mods/errors.swiftmodule -experimental-allow-module-with-compiler-errors %t/mods/errorsmain.partial.swiftmodule %t/mods/errorsempty.partial.swiftmodule -emit-module-source-info -emit-module-doc -emit-dependencies-path %t/mods/errors.d -emit-objc-header -emit-objc-header-path %t/mods/errors.h -emit-module-interface-path %t/mods/errors.swiftinterface -emit-tbd-path %t/mods/errors.tbd

// RUN: llvm-bcanalyzer %t/mods/errors.swiftmodule | %FileCheck -check-prefix=CHECK-BC %s
// RUN: ls %t/mods/errorsmain.partial.swiftdeps %t/mods/errors.d %t/mods/errors.swiftsourceinfo %t/mods/errors.swiftdoc %t/mods/errors.h
// RUN: not ls %t/mods/errors.swiftinterface
// RUN: not ls %t/mods/errors.tbd
// CHECK-BC-NOT: UnknownCode

public func invalid() -> undefined {} // expected-error {{cannot find type 'undefined'}}
