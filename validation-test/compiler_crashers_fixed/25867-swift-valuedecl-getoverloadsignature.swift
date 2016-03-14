// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol a{protocol A:b}enum S{struct Q{struct Q{enum A{protocol A:d{class A
