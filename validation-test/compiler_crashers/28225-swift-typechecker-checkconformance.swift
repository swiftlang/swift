// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
func e{enum k:A}protocol A{associatedtype B:A
associatedtype n
