// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -verify %s
// REQUIRES: objc_interop

import Foundation

struct Wrapper {
    let closure: @convention(c) ([Int]) -> Void

    func callIt() {
        self.closure([])
    }
}
REQUIRES: updating_for_owned_noescape
