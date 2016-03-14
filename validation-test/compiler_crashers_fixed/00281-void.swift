// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct d<c : e, d: e where d.e == c(c:<b>) {
    }
}
d
