//===-- TensorElementProtocol.swift ---------------------------*- swift -*-===//
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
//
// This file defines the TensorElementProtocol and related helpers.
//
//===----------------------------------------------------------------------===//

import Swift

public protocol TensorElementProtocol {
}

extension Bool : TensorElementProtocol {}
extension Int8 : TensorElementProtocol {}
extension UInt8 : TensorElementProtocol {}
extension Int16 : TensorElementProtocol {}
extension UInt16 : TensorElementProtocol {}
extension Int32 : TensorElementProtocol {}
extension UInt32 : TensorElementProtocol {}
extension Int64 : TensorElementProtocol {}
extension UInt64 : TensorElementProtocol {}
extension Int : TensorElementProtocol {}
extension UInt : TensorElementProtocol {}
extension Float : TensorElementProtocol {}
extension Double : TensorElementProtocol {}
