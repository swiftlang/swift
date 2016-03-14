// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct d<T where g:a{class B<a{
class a{var:{
func g<T where H:b
struct Ba{
class B:a
