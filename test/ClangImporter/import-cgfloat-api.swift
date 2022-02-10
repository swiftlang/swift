// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/modules

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t/modules) -Xcc -DCGFLOAT_IN_COREFOUNDATION -DCGFLOAT_IN_COREFOUNDATION -emit-module -o %t/modules/CoreFoundation.swiftmodule %clang-importer-sdk-path/swift-modules/CoreFoundation.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t/modules) -Xcc -DCGFLOAT_IN_COREFOUNDATION -DCGFLOAT_IN_COREFOUNDATION -emit-module -o %t/modules/CoreGraphics.swiftmodule %clang-importer-sdk-path/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t/modules) -Xcc -DCGFLOAT_IN_COREFOUNDATION -DCGFLOAT_IN_COREFOUNDATION -emit-module -o %t/modules/Foundation.swiftmodule %clang-importer-sdk-path/swift-modules/Foundation.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t/modules) -typecheck -Xcc -DCGFLOAT_IN_COREFOUNDATION -DCGFLOAT_IN_COREFOUNDATION %s

// REQUIRES: objc_interop

import CoreFoundation
import Foundation
import CoreGraphics

func test() -> UnsafeMutablePointer<CGFloat>? {
    let color = CGColor(gray: 1.0, alpha: 1.0)
    return CGColorGetComponents(color)
}

