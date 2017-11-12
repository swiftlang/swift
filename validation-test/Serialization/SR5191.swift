// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name SwiftCrash -emit-module -o %t/SR5191.swiftmodule %s %S/Inputs/SR5191-other.swift
// RUN: %target-build-swift -module-name SwiftCrash -emit-module -o %t/SR5191_reversed.swiftmodule %S/Inputs/SR5191-other.swift %s

// REQUIRES: objc_interop
// The module name is significant here; it must be later ASCIIbetically than
// "Swift". This has to do with the canonical ordering of protocols, including
// those inherited by extending NSObject.

import Foundation

class FooImpl: NSObject, FooProto, BazProto {
  required init(bar: BarImpl) {}
}
