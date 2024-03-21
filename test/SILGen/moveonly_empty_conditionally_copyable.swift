// RUN: %target-swift-frontend -enable-experimental-feature NoncopyableGenerics -emit-sil -verify -primary-file %s

struct G<T: ~Copyable>: ~Copyable { }

extension G: Copyable {}
