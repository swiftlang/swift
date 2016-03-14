// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class a
struct c<s where g:A{var _=a<c
class b{class B<d
var f=B
