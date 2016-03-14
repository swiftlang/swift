// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{
struct S<T{class
a
struct d
{
struct B<T,U{
class a{struct A<T{
class d<T{
class B:a
{var d
