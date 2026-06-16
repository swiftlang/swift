// RUN: %target-swift-frontend -I %S/Inputs %s -emit-sil -O -cxx-interoperability-mode=default -disable-availability-checking | %FileCheck %s

import ImmortalFRTHashable

extension ImmortalFRT: Equatable {
    public static func ==(lhs: ImmortalFRT, rhs: ImmortalFRT) -> Bool { true }
}

extension ImmortalFRT: Hashable {
    public func hash(into hasher: inout Hasher) {
        _ = getHashValue(self)
    }
}

extension UnsafeFRT: Equatable {
    public static func ==(lhs: UnsafeFRT, rhs: UnsafeFRT) -> Bool { true }
}

extension UnsafeFRT: Hashable {
    public func hash(into hasher: inout Hasher) {
        _ = getHashValue(self)
    }
}

// CHECK-LABEL: sil {{.*}} @{{.*}}_rawHashValue{{.*}}ImmortalFRT
// CHECK-NOT: load [trivial] {{.*}} : $*τ_0_0
// CHECK: end sil function

// CHECK-LABEL: sil {{.*}} @{{.*}}_rawHashValue{{.*}}UnsafeFRT
// CHECK-NOT: load [trivial] {{.*}} : $*τ_0_0
// CHECK: end sil function
