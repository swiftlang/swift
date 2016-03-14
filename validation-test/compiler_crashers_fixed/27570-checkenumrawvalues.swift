// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func u{struct c<T, U{class d?enum S<e{
enum B<T> :d
