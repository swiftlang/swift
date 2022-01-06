// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

import Foundation

let itemsPerRow = 10
let size: CGFloat = 20
let margin: CGFloat = 10

let _ = (0..<100).map { (row: CGFloat($0 / itemsPerRow), col: CGFloat($0 % itemsPerRow)) }
                 .map {
                        CGRect(x: $0.col * (size + margin) + margin,
                               y: $0.row * (size + margin) + margin,
                               width: size,
                               height: size)
                 }
