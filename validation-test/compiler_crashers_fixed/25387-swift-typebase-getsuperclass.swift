// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class a
enum S<T where S=b{struct c<T{
class b:a{
let l
