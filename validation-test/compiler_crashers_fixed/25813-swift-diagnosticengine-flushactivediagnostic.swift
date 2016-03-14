// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol a{{}protocol B{}struct B<U:B{class d{func a(}}struct B<T:T.r
