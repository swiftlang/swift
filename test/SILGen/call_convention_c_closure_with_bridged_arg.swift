// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -verify %s
// REQUIRES: objc_interop

import Foundation

struct Wrapper {
    let closure: @convention(c) ([Int]) -> Void

    func callIt() {
        self.closure([])
    }
}
