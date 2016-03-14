// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{class A{let a{{let:A{{A:{t v{class d{let a{{{a d{class c{func b{c{let:{{f=b
