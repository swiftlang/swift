// RUN: %target-swift-frontend -emit-sil -disable-experimental-parser-round-trip -disable-availability-checking -enable-experimental-feature BuiltinModule %s

// REQUIRES: swift_feature_BuiltinModule

import Builtin

struct MyVector<let N: Int, T: ~Copyable>: ~Copyable {
    var storage: Builtin.FixedArray<N, T>

    init() {
        // make sure that `Builtin.emplace` considers the result storage
        // initialized, even if the closure doesn't actually do anything
        // with the pointer
        self.storage = Builtin.emplace { ptr in
        }
    }
}

