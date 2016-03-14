// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class f<p : k, p : k where p.n == pealias n
}
(f() as n).m.k()
