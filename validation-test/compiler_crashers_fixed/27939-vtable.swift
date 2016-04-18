// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
// ASAN Output: stack-overflow on address 0x7ffdad0b1cd0 (pc 0x000001cf1268 bp 0x7ffdad0b2650 sp 0x7ffdad0b1c60 T0)
func b<T {
class A : A.e {
func e: T.e
