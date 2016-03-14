// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
if{
class c<I{
class a
class A:a{let a
class A{class A:a
