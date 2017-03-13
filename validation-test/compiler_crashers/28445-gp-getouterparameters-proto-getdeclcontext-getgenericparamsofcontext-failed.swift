// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -emit-ir
// FIXME(huonw): where clauses on associatedtypes broke this
// XFAIL: *
// REQUIRES: asserts
class A{let f= <c
protocol A{
typealias e:A{}
protocol A{
extension{
protocol A{func<{{}
}typealias e:
c
