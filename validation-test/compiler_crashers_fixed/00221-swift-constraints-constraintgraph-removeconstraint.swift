// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct i<d, k: f where d.i == k>h i<i   k , d>
func f<k>(k : k) -> i
