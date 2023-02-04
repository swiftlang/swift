// RUN: %empty-directory(%t)

// ---- (1) Build a simple library
// RUN: echo "public func theAnswer() -> Int { 42 }" > %t/helper.swift
// RUN: %target-build-swift-dylib(%t/%target-library-name(helper)) \
// RUN:   -emit-module-path %t/helper.swiftmodule \
// RUN:   -parse-as-library -enable-library-evolution \
// RUN:   %t/helper.swift

// ---- (2) Build executable
// RUN: %target-build-swift -emit-executable %s -g -o %t/test \
// RUN:   -I %t/ -L %t/ %target-rpath(%t/) -lhelper
// RUN: %target-codesign %t/test

// ---- (3) Run the executable
// RUN: %target-run %t/test %t/%target-library-name(helper) | %FileCheck %s --check-prefix=HAS-ANSWER

// ---- (4) Re-build the library with empty input
// RUN: echo "" > %t/empty.swift
// RUN: %target-build-swift-dylib(%t/%target-library-name(helper)) \
// RUN:   -emit-module-path %t/helper.swiftmodule \
// RUN:   -parse-as-library -enable-library-evolution \
// RUN:   %t/empty.swift

// ---- (5) Run the executable
// RUN: %target-run %t/test %t/%target-library-name(helper) | %FileCheck %s --check-prefix=NO-ANSWER

// REQUIRES: executable_test
// UNSUPPORTED: OS=windows-msvc

// This test requires executable tests to be run on the same machine as the
// compiler, as it links with a dylib that it doesn't arrange to get uploaded
// to remote executors.
// UNSUPPORTED: remote_run || device_run

@_weakLinked import helper

// HAS-ANSWER: 42
// NO-ANSWER: No answer
if #_hasSymbol(theAnswer) {
  print(theAnswer())
} else {
  print("No answer")
}
