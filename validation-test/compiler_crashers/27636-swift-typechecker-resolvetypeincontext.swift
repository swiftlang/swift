// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
// ASAN Output: stack-overflow on address 0x7ffdcd2b1fd0 (pc 0x0000008ecf9e bp 0x7ffdcd2b2810 sp 0x7ffdcd2b1fc0 T0)
enum A
protocol A{
associatedtype f:a
func a<T where f:d
