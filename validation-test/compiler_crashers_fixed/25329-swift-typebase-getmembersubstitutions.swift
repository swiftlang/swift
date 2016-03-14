// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct g<T where g=d{
class B<g{
struct B
struct B
var _=B
