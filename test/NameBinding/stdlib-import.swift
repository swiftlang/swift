// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: echo > %t/swift.swift
// RUN: %swift %s -parse -verify
// RUN: %swift %s -parse -parse-stdlib
// RUN: rm %t/swift.swift

// RUN: echo > %t/swift.swiftmodule
// RUN: %swift %s -parse -verify
// RUN: %swift %s -parse -parse-stdlib
// RUN: rm %t/swift.swiftmodule

import swift

struct IntWrapper {
  var value: swift.Int
}
