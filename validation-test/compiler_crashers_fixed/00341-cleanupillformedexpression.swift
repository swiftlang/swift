// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
func f<T>() -> T -> T {
return { x in x 1 {
b[c][c] = 1
}
}
class A {
class func a() -> String {
let d: String = {
self.a()
}()
