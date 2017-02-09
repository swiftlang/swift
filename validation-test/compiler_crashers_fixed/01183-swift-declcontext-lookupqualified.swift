// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
class c {
func b((Any, c))(Any, AnyObject
}
struct A<T> {
}
struct c<S: Sequence, T where Optional<T> == S.Iterator.Element>(xs
