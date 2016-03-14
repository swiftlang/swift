// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
struct B<w{
class d:A
class A protocol c{
typealias e:d
typealias e:AnyObject
