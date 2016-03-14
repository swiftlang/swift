// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{struct Q{struct B<T where g:A{class b{class B:b{protocol d
