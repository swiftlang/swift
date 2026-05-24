// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action extract-imports %t/input.swift > %t/output.swift
// RUN: %diff %t/output.swift %t/expected.swift

//--- input.swift
import Foundation
import Swift
@_exported import CoreGraphics
public import PublicMod
internal import InternalMod
@_implementationOnly import HiddenMod

public func publicFunc() {
  print("hello")
}

internal func internalFunc() {}

public struct Foo {
  public var x: Int
}

public class Bar {}

public protocol Baz {}
//--- expected.swift
import Foundation
import Swift
@_exported import CoreGraphics
public import PublicMod
internal import InternalMod
@_implementationOnly import HiddenMod
