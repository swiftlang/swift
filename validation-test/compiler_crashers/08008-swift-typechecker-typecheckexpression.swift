// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
// ASAN Output: stack-overflow on address 0x7fffe2a98fd8 (pc 0x000001e12adb bp 0x7fffe2a992d0 sp 0x7fffe2a98f60 T0)
class A:A.b{let b=Void{
