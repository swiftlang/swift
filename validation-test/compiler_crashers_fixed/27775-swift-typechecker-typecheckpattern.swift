// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{if{enum H{var T}}}class d<T where H:S{class A{var f:T
