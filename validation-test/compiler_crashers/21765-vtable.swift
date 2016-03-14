// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
// ASAN Output: stack-overflow on address 0x7ffe8def3f70 (pc 0x000001cf1268 bp 0x7ffe8def48f0 sp 0x7ffe8def3f00 T0)
func b<T {
class A : A.e {
func e: T.e
