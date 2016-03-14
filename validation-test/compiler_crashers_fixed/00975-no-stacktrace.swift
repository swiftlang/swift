// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
enum S<U, length: Sequence where S.c : B<c> (start, AnyObject) -> U)
protocol d
