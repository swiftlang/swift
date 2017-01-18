// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
class a<f : b, g : b where f.d == g> {
}
protocol b {
typealias d
}
struct c<h : b> : b {
typealias e = a<c<h>, d>
}
struct c<S: Sequence, T where Optional<T> == S.Iterator.Element
