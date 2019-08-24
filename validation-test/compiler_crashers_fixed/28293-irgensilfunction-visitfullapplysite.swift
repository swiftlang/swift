// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -emit-sil

// Submitted by https://github.com/deville (Andrii Chernenko)

func f(value: Any) {
}

struct A {
}

Mirror(reflecting: A()).children.forEach(f)
