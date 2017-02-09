// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
func p(l: Any, g: Any) -> (((Any, Any) -> Any) -> Any) {
return {
(p: (Any, Any) -> Any) -> Any in a(b: Int = 0) {
}
protocol A {
}
lett D : C {
func g<T where T.E == F>(f: B<T>) {
}
}
struct d<f : e, g: e where g.h == f.h> {
col P {
}
f b {
}
struct d<d : n, o:
