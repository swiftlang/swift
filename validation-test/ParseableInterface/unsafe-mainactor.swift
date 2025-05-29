// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

// REQUIRES: OS=macosx

import AppKit

// CHECK: @objc @_inheritsConvenienceInitializers @_Concurrency.MainActor @preconcurrency public class Subclass : AppKit.NSView {
public class Subclass: NSView {
  // CHECK: @_Concurrency.MainActor @preconcurrency @objc override dynamic public init(frame frameRect: Foundation.NSRect)
  // CHECK: @_Concurrency.MainActor @preconcurrency @objc required dynamic public init?(coder: Foundation.NSCoder)
}
