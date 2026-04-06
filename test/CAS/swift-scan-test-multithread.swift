// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Test parallel dependency scanning from libSwiftScan APIs.
 // RUN: %swift-scan-test -action scan_dependency -cas-path %t/cas -threads 5 -- \
// RUN:   %target-swift-frontend -scan-dependencies -target arm64-apple-macos26 -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/deps.json -sdk %sdk -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include > %t/deps.json

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json A | %FileCheck %s --check-prefix=INTERFACE_A
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:B | %FileCheck %s --check-prefix=PCM_B
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test | %FileCheck %s --check-prefix=TEST

// INTERFACE_A: -frontend
// INTERFACE_A: -compile-module-from-interface

// PCM_B: -frontend
// PCM_B: -emit-pcm

// TEST: -direct-clang-cc1-module-build

//--- include/module.modulemap
module B {
  header "B.h"
  export *
}

//--- include/B.h
void notused(void);

//--- include/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
import B

public struct A {}

//--- main.swift
import A
