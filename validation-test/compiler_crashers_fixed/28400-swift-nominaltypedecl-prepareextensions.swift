// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -typecheck
// Discovered by https://github.com/airspeedswift (airspeedswift)

let s1: Set<Int>? = []
let s2: Set<Int>? = []
let s3: Set<Int>? = []
let x = s1 ?? s2 ?? s3 ?? []
