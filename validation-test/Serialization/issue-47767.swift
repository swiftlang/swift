// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name SwiftCrash -emit-module -o %t/M.swiftmodule %s %S/Inputs/issue-47767-other.swift
// RUN: %target-build-swift -module-name SwiftCrash -emit-module -o %t/M_reversed.swiftmodule %S/Inputs/issue-47767-other.swift %s

// REQUIRES: objc_interop
// The module name is significant here; it must be later ASCIIbetically than
// "Swift". This has to do with the canonical ordering of protocols, including
// those inherited by extending NSObject.

// https://github.com/apple/swift/issues/47767

import Foundation

class FooImpl: NSObject, FooProto, BazProto {
  required init(bar: BarImpl) {}
}
