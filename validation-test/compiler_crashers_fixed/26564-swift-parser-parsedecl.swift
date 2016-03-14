// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct Q{struct B<T where B:a{{}struct B{struct S{func a{return[T:a{}var d{class a{:}}
