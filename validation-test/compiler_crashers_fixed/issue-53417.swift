// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -emit-silgen

// https://github.com/apple/swift/issues/53417

func block1(_ f: @convention(block) @autoclosure () -> Int) -> Void {}
block1(1)

func c1(_ f: @convention(c) @autoclosure () -> Int) -> Void {}
c1(1)

func swift1(_ f: @convention(swift) @autoclosure () -> Int) -> Void {} // OK
swift1(1)

func thin1(_ f: @convention(thin) @autoclosure () -> Int) -> Void {} // OK
thin1(1)

func block2(_ f: @autoclosure @convention(block) () -> Int) -> Void {}
block2(1)

func c2(_ f: @autoclosure @convention(c) () -> Int) -> Void {}
c2(1)

func swift2(_ f: @autoclosure @convention(swift) () -> Int) -> Void {} // OK
swift2(1)

func thin2(_ f: @autoclosure @convention(thin) () -> Int) -> Void {} // OK
thin2(1)
