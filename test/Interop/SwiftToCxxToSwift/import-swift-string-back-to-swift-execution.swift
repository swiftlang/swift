// RUN: %empty-directory(%t)
// RUN: %target-swiftxx-frontend %S/Inputs/swift-string.swift -module-name StringBridge \
// RUN:   -typecheck -emit-clang-header-path %t/StringBridge.h
// RUN: %target-swiftxx-frontend %S/Inputs/swift-string-client.swift -emit-module \
// RUN:   -module-name StringClient -emit-module-path %t/StringClient.swiftmodule -I %t \
// RUN:   -Xcc -fmodule-map-file=%S/Inputs/swift-string.modulemap -Xcc -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR \
// RUN:   -enable-experimental-feature CxxSwiftValueTypes
// RUN: %target-interop-build-swift %s -I %t \
// RUN:   -Xcc -fmodule-map-file=%S/Inputs/swift-string.modulemap -Xcc -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR \
// RUN:   -enable-experimental-feature CxxSwiftValueTypes -o %t/round-trip
// RUN: %target-codesign %t/round-trip
// RUN: %target-run %t/round-trip | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxSwiftValueTypes

import SwiftStringRoundTrip
import StringClient

func check(_ actual: Swift.String, _ expected: Swift.String) {
  precondition(actual == expected)
}

check(makeString(), "from C++")
hello("hello")

let methods = StringFunctions()
let values: [Swift.String] = ["", "small", "Grüße 🌍", "a\0b", Swift.String(repeating: "heap", count: 100)]
for value in values {
  check(echoString(value), value)
  check(borrowString(value), value)
  check(echoAlias(value), value)
  check(mixedStrings(1, value, "other"), value)
  check(mixedStrings(0, "other", value), value)
  check(Strings.echo(value), value)
  check(methods.echo(value), value)
  check(mutateCopy(value), value + "!")
  check(consumeString(consuming: value), value + "!")
  check(borrowConstRValue(consuming: value), value)
  check(serializedRoundTrip(value), value + "!")

  var mutated = value
  mutateString(&mutated)
  check(mutated, value + "!")
  check(echoString(value), value)
}
check(StringFunctions.make(), "from C++")
check(defaultString(), "default")
check(defaultString("explicit"), "explicit")
check(StringHolder().echo("member"), "member")

let function = echoString
check(function("through a Swift function value"), "through a Swift function value")

for index in 0..<1000 {
  let value = Swift.String(repeating: "heap-backed-\(index)-", count: 50)
  check(mutateCopy(value), value + "!")
  check(echoString(value), value)
}

print("Swift String round trips passed")
// CHECK: Swift String round trips passed
