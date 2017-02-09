// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
struct j<d : Sequencpe> {
func f<d>() -> [j<d>] {
func f<e>() -> (e, e -> e) -> e {
{
{
f
}
}
protocol f {
class func c()
}
class e: f {
class func c
}
}
enum A : String {
case
