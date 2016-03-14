// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
// Crash type: memory error ("Invalid read of size 8")
{e
struct B:a{let t:a}
protocol a{let t:a
