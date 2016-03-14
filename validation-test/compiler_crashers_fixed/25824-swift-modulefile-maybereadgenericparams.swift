// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol a{
typealias e:b
typealias b{
}protocol A{typealias B:A
protocol A{typealias e:b}
}
let h:A
