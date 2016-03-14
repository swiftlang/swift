// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
// Crash type: memory error ("Invalid read of size 4")
var a{class d{var b=B{}let c=(x:d<T{{}}class B<T where h=d>:a
