// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module-path %t/main4.swiftmodule -swift-version 4 %s
// RUN: %target-build-swift -emit-module-path %t/main4_2.swiftmodule -swift-version 4.2 %s

// REQUIRES: OS=ios

import UIKit

public func testInsets(_: UIEdgeInsets = .zero) {}
public func testOffsets(_: UIOffset = .zero) {}
