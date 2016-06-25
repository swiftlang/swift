// RUN: not --crash %target-swift-frontend %s -emit-silgen

enum X {
    init?(a: Int) {
        .p[a]
    }
}
