// RUN: %target-swift-emit-silgen %s -target %target-cpu-apple-macosx11 -swift-version 5

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

protocol StaticFactory {
    associatedtype Result

    func callAsFunction() -> Self.Result
}

struct ViewFactory: StaticFactory {
    let systemName: String
    
    func callAsFunction() -> some View {
        Image(systemName: systemName)
            .frame(width: 20, height: 20)
    }
}

extension Button where Label == ViewFactory.Result {
    init(
        systemName: String,
        action: @escaping () -> Void
    ) {
        self.init(
            action: action
        ) {
            ViewFactory(systemName: systemName)()
        }
    }
}
