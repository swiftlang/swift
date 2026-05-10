// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100
// REQUIRES: objc_interop

// https://github.com/swiftlang/swift/issues/57515

import Foundation

// not necessary but avoids dependency on existing result builders
@resultBuilder
enum Builder<T> {
    static func buildBlock(_ components: Never...) -> Never {
        switch components.first! {}
    }
}

@Builder<Never> func compute() -> Never {
// alternatively, if you'd rather avoid the custom builder:
//@ViewBuilder func compute() -> some View {
    let width: CGFloat = 100.0
    let spacePerBox = width + width
    let _ = floor(spacePerBox + spacePerBox)
    let _ = width / spacePerBox
    let _ = width / spacePerBox
    let _ = width / spacePerBox
    let _ = width / spacePerBox
    let _ = width / spacePerBox
    let _ = width / spacePerBox
    let _ = width / spacePerBox
    let _ = width / spacePerBox
    let _ = width / spacePerBox
    // each additional one of these lines doubles compile times
}
