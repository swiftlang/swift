// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O -I %t/include \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas -profile-coverage-mapping -profile-generate

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/A.cmd

// RUN: %FileCheck %s --input-file=%t/A.cmd

// CHECK: -direct-clang-cc1-module-build
// CHECK-NOT: -fcoverage-compilation-dir

//--- main.swift
import A
func test() {
  a();
}

//--- include/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func a() { }
