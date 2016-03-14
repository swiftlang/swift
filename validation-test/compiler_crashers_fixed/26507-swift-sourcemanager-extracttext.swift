// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct S<T where a:B{struct Q<T{enum a{class b{{}struct B<T where H:A}}struct B:A{}protocol A
