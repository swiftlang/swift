// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func f{
enum S:N
enum S<T where g:N{
enum k:A{
enum a=class B:a
