// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var f{if{enum A{protocol a}}class A{struct B<f:f.c
