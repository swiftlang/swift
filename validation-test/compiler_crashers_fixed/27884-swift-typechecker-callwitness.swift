// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
_=[]:{struct d{struct d<T where T:a{struct B{enum B<T{let a=B{
