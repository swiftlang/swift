// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
// Crash type: memory error ("Invalid read of size 4")
class n{protocol a:d var d={class b:a
