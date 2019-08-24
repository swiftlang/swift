// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
// ASAN Output: stack-overflow on address 0x7fffe2a98fd8 (pc 0x000001e12adb bp 0x7fffe2a992d0 sp 0x7fffe2a98f60 T0)
class A:A.b{let b=Void{
