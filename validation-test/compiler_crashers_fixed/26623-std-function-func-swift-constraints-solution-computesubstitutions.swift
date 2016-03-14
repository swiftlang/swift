// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B<T{class c{struct B<T where g:A{class B{var f{g{}}func g}}}class c{let h:Collection
