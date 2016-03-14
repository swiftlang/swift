// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{struct B{struct Q{struct T{class a{struct B{struct A{struct A<T where g:C{struct A{var d{A{
