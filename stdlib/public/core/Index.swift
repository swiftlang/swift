//===--- Index.swift - A position in a Collection -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// This protocol is an implementation detail of `Integer`; do not use it
/// directly.
@_show_in_interface
public protocol _Incrementable : Equatable {}

@available(*, unavailable, message: "Use \'-= 1\' or call collection.prior(Index)")
@discardableResult
public prefix func -- <T : _Incrementable>(i: inout T) -> T {
  Builtin.unreachable()
}

@available(*, unavailable, message: "Use \'-= 1\' or call collection.prior(Index)")
@discardableResult
public postfix func -- <T : _Incrementable>(i: inout T) -> T {
  Builtin.unreachable()
}

@available(*, unavailable, message: "Use \'+= 1\' or call 'collection.index(after: Index)")
@discardableResult
public prefix func ++ <T : _Incrementable>(i: inout T) -> T {
  Builtin.unreachable()
}

@available(*, unavailable, message: "Use \'+= 1\' or call 'collection.index(after: Index)")
@discardableResult
public postfix func ++ <T : _Incrementable>(i: inout T) -> T {
  Builtin.unreachable()
}

@available(*, unavailable, renamed: "Comparable")
public typealias ForwardIndexType = Void

@available(*, unavailable, renamed: "Comparable")
public typealias BidirectionalIndexType = Void

@available(*, unavailable, renamed: "Strideable")
public typealias RandomAccessIndexType = Void
