// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct a{init(){struct S<T where g:A{class b<a{class a{{}}let:a
