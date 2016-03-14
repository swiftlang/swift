// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{
a _ <T
class A<T where g: a {
enum S<d {
struct E{
struct dT{
class A : A
