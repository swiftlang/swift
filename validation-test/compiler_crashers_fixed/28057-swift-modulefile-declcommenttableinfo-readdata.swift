// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class S<T{
enum b:A
func a{
protocol B{
func f<T where g=e
func g:T.C
