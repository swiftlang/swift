// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let c{protocol A:A{protocol a{protocol a{}typealias e:a{}typealias e
