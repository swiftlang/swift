// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol f{struct S<T:y}struct d{struct w{protocol c
