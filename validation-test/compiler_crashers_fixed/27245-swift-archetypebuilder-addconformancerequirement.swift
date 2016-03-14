// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func a<H{protocol P{
func f:P
func c:
g
class B<T where H.h=c
