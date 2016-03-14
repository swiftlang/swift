// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B{let n=[]struct Q<T where g:a{struct c{{}func a{(a
