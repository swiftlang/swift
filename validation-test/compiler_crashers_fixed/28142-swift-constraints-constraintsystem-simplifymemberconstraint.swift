// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{
class a{class b:Collection
}enum B<T where B:a{{}class b<a
class b<T where g:A{var d=b(
