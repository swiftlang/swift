// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{struct Q{protocol a{class T func f:T{}}}class A{class A{func g<f:f.c
