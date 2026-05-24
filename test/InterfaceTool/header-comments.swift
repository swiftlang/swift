// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action minimize-source %t/swiftinterface.swift > %t/si-output.swift
// RUN: %diff %t/si-output.swift %t/si-expected.swift
// RUN: %swift-interface-tool -action extract-imports %t/swiftinterface.swift > %t/si-imports-output.swift
// RUN: %diff %t/si-imports-output.swift %t/si-imports-expected.swift
// RUN: %swift-interface-tool -action minimize-source %t/license.swift > %t/lic-output.swift
// RUN: %diff %t/lic-output.swift %t/lic-expected.swift

//--- swiftinterface.swift
// swift-interface-format-version: 1.0
// swift-compiler-version: Apple Swift version 6.0
// swift-module-flags: -target arm64-apple-macos14.0 -enable-library-evolution -module-name MyModule
import Swift
import Foundation

/// Doc comment on public func.
public func publicFunc() {
  print("hello")
}

// Regular comment before private func.
private func privateFunc() {
  print("private")
}
//--- si-expected.swift
// swift-interface-format-version: 1.0
// swift-compiler-version: Apple Swift version 6.0
// swift-module-flags: -target arm64-apple-macos14.0 -enable-library-evolution -module-name MyModule
import Swift
import Foundation

public func publicFunc()

private func privateFunc()
//--- si-imports-expected.swift
// swift-interface-format-version: 1.0
// swift-compiler-version: Apple Swift version 6.0
// swift-module-flags: -target arm64-apple-macos14.0 -enable-library-evolution -module-name MyModule
import Swift
import Foundation
//--- license.swift
//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
//===----------------------------------------------------------------------===//
import Foundation

/// Doc comment on public func.
public func publicFunc() {
  print("hello")
}

private func privateFunc() {}
//--- lic-expected.swift
import Foundation

public func publicFunc()

private func privateFunc()
