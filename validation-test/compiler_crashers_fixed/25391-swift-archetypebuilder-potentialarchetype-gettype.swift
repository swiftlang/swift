// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var B{class A{struct B<T where k=o{class A:A{{}}}protocol A{typealias b
