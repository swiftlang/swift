// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
// ASAN Output: stack-overflow on address 0x7ffdad0b1cd0 (pc 0x000001cf1268 bp 0x7ffdad0b2650 sp 0x7ffdad0b1c60 T0)
func b<T {
class A : A.e {
func e: T.e
