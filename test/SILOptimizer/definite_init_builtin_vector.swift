// RUN: %target-swift-frontend -emit-sil -disable-availability-checking -enable-experimental-feature ValueGenerics -enable-experimental-feature BuiltinModule %s

import Builtin

struct MyVector<let N: Int, T: ~Copyable>: ~Copyable {
    var storage: Builtin.Vector<N, T>

    init() {
        // make sure that `Builtin.emplace` considers the result storage
        // initialized, even if the closure doesn't actually do anything
        // with the pointer
        self.storage = Builtin.emplace { ptr in
        }
    }
}

