// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func i(f: g) -> <j>(() -> j) -> g { func g
k, l {
    typealias l = m<k<m>, f>
}
