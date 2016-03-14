// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class T{{}init(){struct Q<a{class B:A{class d:d}class A
