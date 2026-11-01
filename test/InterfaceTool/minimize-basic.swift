// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action minimize %t/input.swift > %t/output.swift
// RUN: %diff -u %t/expected.swift %t/output.swift

// Minimization should be idempotent.
// RUN: %swift-interface-tool -action minimize %t/output.swift > %t/output-again.swift
// RUN: %diff -u %t/output.swift %t/output-again.swift

// Minimized output should parse.
// RUN: %target-swift-frontend -parse %t/output.swift

//--- input.swift
import AlphaMod
import BetaMod
@_exported import GammaMod
public import PublicMod
internal import InternalMod
@_implementationOnly import HiddenMod

@testable import TestableMod
@preconcurrency import PreconcurrencyMod
@_spi(SomeSPI) import SPIMod
@_spiOnly import SPIOnlyMod
@_weakLinked import WeakLinkedMod
@_documentation(visibility: internal) import DocumentedMod
@_private(sourceFile: "Secret.swift") import PrivateSourceMod
package import PackageMod
private import PrivateMod
fileprivate import FilePrivateMod

import struct Lib.MyStruct
import class Lib.MyClass
import protocol Lib.MyProtocol
import enum Lib.MyEnum
import func Lib.myFunc
import var Lib.myVar
import typealias Lib.MyAlias

@_implementationOnly
import SplitAttr

import   ExtraSpaces
import /* block comment */ WithBlockComment
import WithLineComment // trailing line comment

public func publicFunc() {
  print("hello")
}

internal func internalFunc() {}

public struct Foo {
  public var x: Int
}

public class Bar {}
public protocol Baz {}
public enum Qux { case a, b }
public actor MyActor {}
public typealias TopAlias = Int

public extension Int {
  func extra() {}
}

public var topVar: Int = 0
public let topLet = 1

infix operator <+> : AdditionPrecedence
precedencegroup MyPrec { higherThan: AdditionPrecedence }
//--- expected.swift
import AlphaMod
import BetaMod
@_exported import GammaMod
public import PublicMod
internal import InternalMod
@_implementationOnly import HiddenMod
@testable import TestableMod
@preconcurrency import PreconcurrencyMod
@_spi(SomeSPI) import SPIMod
@_spiOnly import SPIOnlyMod
@_weakLinked import WeakLinkedMod
@_documentation(visibility: internal) import DocumentedMod
@_private(sourceFile: "Secret.swift") import PrivateSourceMod
package import PackageMod
private import PrivateMod
fileprivate import FilePrivateMod
import struct Lib.MyStruct
import class Lib.MyClass
import protocol Lib.MyProtocol
import enum Lib.MyEnum
import func Lib.myFunc
import var Lib.myVar
import typealias Lib.MyAlias
@_implementationOnly import SplitAttr
import ExtraSpaces
import WithBlockComment
import WithLineComment
