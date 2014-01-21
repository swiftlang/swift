//===-- PerformanceHacks.swift - Unfortunate code, for better performance -===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file consists of unfortunate code that should not need to exist, which
// we include to get better runtime performance of Swift apps.
//
//===----------------------------------------------------------------------===//

@prefix @assignment func ++(x : @inout Int8) -> Int8 { x = x + 1; return x }
@prefix @assignment func --(x : @inout Int8) -> Int8 { x = x - 1; return x }
@prefix @assignment func ++(x : @inout Int16) -> Int16 { x = x + 1; return x }
@prefix @assignment func --(x : @inout Int16) -> Int16 { x = x - 1; return x }
@prefix @assignment func ++(x : @inout Int32) -> Int32 { x = x + 1; return x }
@prefix @assignment func --(x : @inout Int32) -> Int32 { x = x - 1; return x }
@prefix @assignment func ++(x : @inout Int64) -> Int64 { x = x + 1; return x }
@prefix @assignment func --(x : @inout Int64) -> Int64 { x = x - 1; return x }

@prefix @assignment func ++(x : @inout UInt8) -> UInt8 { x = x + 1; return x }
@prefix @assignment func --(x : @inout UInt8) -> UInt8 { x = x - 1; return x }
@prefix @assignment func ++(x : @inout UInt16) -> UInt16 { x = x + 1; return x }
@prefix @assignment func --(x : @inout UInt16) -> UInt16 { x = x - 1; return x }
@prefix @assignment func ++(x : @inout UInt32) -> UInt32 { x = x + 1; return x }
@prefix @assignment func --(x : @inout UInt32) -> UInt32 { x = x - 1; return x }
@prefix @assignment func ++(x : @inout UInt64) -> UInt64 { x = x + 1; return x }
@prefix @assignment func --(x : @inout UInt64) -> UInt64 { x = x - 1; return x }
