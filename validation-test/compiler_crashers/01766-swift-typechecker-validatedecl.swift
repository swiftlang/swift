// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
protocol a {
protocol A {
}
}
protocol b : a {
protocol c : A {
}
}
for b
