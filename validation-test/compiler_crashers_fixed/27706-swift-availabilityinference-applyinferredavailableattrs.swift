// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let T{{var d:class A{class A<T where f:A{class B{let:{class T{{{}}func j{A}}}}class A{let T{d{{
