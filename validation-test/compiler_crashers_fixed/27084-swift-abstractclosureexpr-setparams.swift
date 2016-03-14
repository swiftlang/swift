// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class c<T where g:a{struct S<T{class d{let:{{}struct S{class a:d
