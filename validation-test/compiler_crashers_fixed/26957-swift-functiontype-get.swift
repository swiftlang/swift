// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let a{let e=[({class B{{{}}class C{var d{b=(}}}func a
