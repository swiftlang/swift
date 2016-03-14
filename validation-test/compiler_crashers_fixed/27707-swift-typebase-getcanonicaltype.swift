// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
if true{{
enum S<T where g:a{
class a{
struct B<T{let e=B
struct B
{}class B
