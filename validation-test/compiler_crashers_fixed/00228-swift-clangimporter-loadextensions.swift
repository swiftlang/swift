// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
func s() -> o {
        r q {
}
protocol p {
  typealias m = q
}
m r : p {
}
func r<s : t, o : p where o.m == s> (s: o) {
}
func r<s : p where s.m == s> (s: s) {
}
m s<p : u> {
}
class t<s : s, r : s where s.t == r> : q {
}
class t<s, r> {
}
protocol s {
    func r() {
    }
}
func t(s) -> <p>(() -> p) {
}
class q<m : t
