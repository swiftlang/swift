// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
enum S:A{class a{enum e{struct Q{{}struct b{protocol d{struct B<T:c
