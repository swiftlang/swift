// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
if{struct B{struct B<T where B:A{class a{let f={}class g:Boolean
