// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
import d{{class a{class A{
enum a<T where g:a
struct X<T:T.a
