// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
if true{var d{class a<T{struct B<T{class a{enum T{var d=B{
