//===--- Function.swift ---------------------------------------------------===//
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
public // @testable
protocol _Function {
  associatedtype Input
  associatedtype Output
  func apply(_: Input) -> Output
}

protocol _Predicate : _Function where Output == Bool { }

struct _Closure<Input, Output> : _Function {
  public func apply(_ input: Input) -> Output {
    return _impl(input)
  }
  let _impl: (Input) -> Output
}

