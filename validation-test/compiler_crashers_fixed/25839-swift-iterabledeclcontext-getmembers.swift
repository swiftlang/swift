// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func a<class a=enum B{class A{enum S{func e:AnyObject}}}class A:a
