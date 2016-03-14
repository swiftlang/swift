// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol d == { x }
class B<l : c<C> String = c] in
protocol c : B<d>
