// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
// ASAN Output: stack-overflow on address 0x7ffdcd2b1fd0 (pc 0x0000008ecf9e bp 0x7ffdcd2b2810 sp 0x7ffdcd2b1fc0 T0)
enum A
protocol A{
associatedtype f:a
func a<T where f:d
