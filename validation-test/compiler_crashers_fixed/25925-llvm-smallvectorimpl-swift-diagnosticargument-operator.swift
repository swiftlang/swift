// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol A{protocol c:c}class A{enum C{protocol A{protocol P{}protocol P
