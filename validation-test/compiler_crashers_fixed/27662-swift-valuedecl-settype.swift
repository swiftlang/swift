// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{==1
enum A{
class a<T where I:a{
struct B{
var _=a
class a<a
class a
