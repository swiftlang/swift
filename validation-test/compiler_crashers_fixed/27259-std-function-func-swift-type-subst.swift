// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class B<T{class A=init(){class a<T where B:C{struct Q<T where B:C{class b:A
