// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -emit-silgen

func sr11027(_ f: @convention(block) @autoclosure () -> Int) -> Void {}
sr11027(1)

func sr11027_c(_ f: @convention(c) @autoclosure () -> Int) -> Void {}
sr11027_c(1)

func sr11027_swift(_ f: @convention(swift) @autoclosure () -> Int) -> Void {} // OK
sr11027_swift(1)

func sr11027_thin(_ f: @convention(thin) @autoclosure () -> Int) -> Void {} // OK
sr11027_thin(1)

func sr11027_2(_ f: @autoclosure @convention(block) () -> Int) -> Void {}
sr11027_2(1)

func sr11027_c_2(_ f: @autoclosure @convention(c) () -> Int) -> Void {}
sr11027_c_2(1)

func sr11027_swift_2(_ f: @autoclosure @convention(swift) () -> Int) -> Void {} // OK
sr11027_swift_2(1)

func sr11027_thin_2(_ f: @autoclosure @convention(thin) () -> Int) -> Void {} // OK
sr11027_thin_2(1)
