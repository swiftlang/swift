// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
print{enum b{class B<d where I:A{
class B<D
var _=B
