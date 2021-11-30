//===----------------------------------------------------------*- swift -*-===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

public protocol _RebasableCollection: Collection {
  init(rebasing slice: SubSequence)
}

extension UnsafeBufferPointer: _RebasableCollection {}
extension UnsafeMutableBufferPointer: _RebasableCollection {}


public protocol _BufferProtocol: Collection  where Index == Int {
  associatedtype Element

  func withMemoryRebound<T, Result>(
    to type: T.Type, _ body: (UnsafeBufferPointer<T>) throws -> Result
  ) rethrows -> Result
}

extension UnsafeBufferPointer: _BufferProtocol {}


public protocol _MutableBaseAddressProtocol: MutableCollection {
  var baseAddress: UnsafeMutablePointer<Element>? { get }
}

extension UnsafeMutableBufferPointer: _MutableBaseAddressProtocol {}


public protocol _MutableBufferProtocol: MutableCollection where Index == Int {
  associatedtype Element

  func initialize(repeating repeatedValue: Element)

  func initialize<S: Sequence>(from source: S) -> (S.Iterator, Index)
    where S.Element == Element

  func initialize<C: Collection>(fromElements: C) -> Index
    where C.Element == Element

  func update(repeating repeatedValue: Element)

  func update<S: Sequence>(
    from source: S
  ) -> (unwritten: S.Iterator, updated: Index) where S.Element == Element

  func update<C: Collection>(
    fromElements: C
  ) -> Index where C.Element == Element

  func moveInitialize(fromElements source: UnsafeMutableBufferPointer<Element>) -> Index

  func moveInitialize(fromElements source: Slice<UnsafeMutableBufferPointer<Element>>) -> Index

  func moveUpdate(fromElements source: UnsafeMutableBufferPointer<Element>) -> Index

  func moveUpdate(fromElements source: Slice<UnsafeMutableBufferPointer<Element>>) -> Index

  func deinitialize() -> UnsafeMutableRawBufferPointer

  func initializeElement(at index: Index, to value: Element)

  func updateElement(at index: Index, to value: Element)

  func moveElement(from index: Index) -> Element

  func deinitializeElement(at index: Index)

  func withMemoryRebound<T, Result>(
    to type: T.Type, _ body: (UnsafeMutableBufferPointer<T>) throws -> Result
  ) rethrows -> Result
}

extension UnsafeMutableBufferPointer: _MutableBufferProtocol {}
