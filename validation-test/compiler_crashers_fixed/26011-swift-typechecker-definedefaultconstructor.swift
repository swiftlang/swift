// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol A{protocol A:A}struct S{struct d{struct d{struct T{struct a
