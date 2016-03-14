// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{
class B<T where B:A{
struct c{
class a{
struct c{
class A{
func a
func c{a{
