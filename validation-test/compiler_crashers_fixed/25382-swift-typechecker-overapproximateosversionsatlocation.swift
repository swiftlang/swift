// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func a{var b{class A{var f=[0}}protocol A{}struct A{let a{A{
