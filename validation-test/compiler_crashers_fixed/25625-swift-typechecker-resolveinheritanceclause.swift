// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct d{enum S<l:A{}enum A{func a{struct S<T where g:a{class a
