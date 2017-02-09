// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
// ASAN Output: stack-overflow on address 0x7ffe8def3f70 (pc 0x000001cf1268 bp 0x7ffe8def48f0 sp 0x7ffe8def3f00 T0)
func b<T {
class A : A.e {
func e: T.e
