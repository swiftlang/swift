// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct S<T{class a{func d{{struct Q<T where g:a{enum S<d where g:A>:a
