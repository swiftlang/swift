// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
protocol A{
class A:a{
}
associatedtype e:a
protocol a{
associatedtype r
}
associatedtype e:A
