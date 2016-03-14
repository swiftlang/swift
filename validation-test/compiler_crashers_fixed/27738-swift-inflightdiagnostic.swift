// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
if{struct A{class B{{}class a<T where g:B{class d{let a=""func a}
