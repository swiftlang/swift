// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/moduleInputs)
// RUN: split-file %s %t

// ERROR: error: unable to resolve Swift module dependency to a compatible module: 'FooBar'
// ERROR: note: found incompatible module '{{.*}}{{/|\\}}moduleInputs{{/|\\}}FooBar.swiftmodule': compiled for a different target platform

// RUN: %target-swift-frontend -emit-module %t/FooBar.swift -emit-module-path %t/moduleInputs/FooBar.swiftmodule -module-name FooBar -target x86_64-unknown-linux-gnu -parse-stdlib
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %t/main.swift -o %t/deps.json -I %t/moduleInputs -I %t/moduleInputs2 -diagnostic-style llvm -scanner-module-validation -target arm64e-apple-macosx11.0 2>&1 | %FileCheck %s -check-prefix=ERROR

//--- main.swift
import FooBar

//--- FooBar.swift
public func fooBar() {}
