// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O -I %t/include \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas -profile-coverage-mapping -profile-generate

// RUN: %if OS=windows-msvc %{ %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SAL > %t/SAL.cmd %}
// RUN: %if OS=windows-msvc %{ %swift_frontend_plain @%t/SAL.cmd %}

// RUN: %if OS=windows-msvc %{ %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:vcruntime > %t/vcruntime.cmd %}
// RUN: %if OS=windows-msvc %{ %swift_frontend_plain @%t/vcruntime.cmd %}

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:_Builtin_stdint > %t/_Builtin_stdint.cmd
// RUN: %swift_frontend_plain @%t/_Builtin_stdint.cmd

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd

// RUN: %FileCheck %s --input-file=%t/shim.cmd
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
