// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol c {
convenience init()
protocol A {
}
protocol P {
}
typealias B<h == b
}
typealias e = { c())
