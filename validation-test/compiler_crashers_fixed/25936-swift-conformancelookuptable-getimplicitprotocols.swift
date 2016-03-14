// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func b{class B{class a<>:B{}enum B enum S<T where B:a{class b
