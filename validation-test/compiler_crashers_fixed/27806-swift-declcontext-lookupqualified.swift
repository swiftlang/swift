// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
if{enum B<T where B:C{class a{class b{let h=c{}class k:b
