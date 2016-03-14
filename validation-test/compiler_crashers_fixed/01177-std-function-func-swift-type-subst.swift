// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class d<j : i, f : i where j.i == f> : e {
}
class d<j, f> {
}
protocol i {
protocol i
