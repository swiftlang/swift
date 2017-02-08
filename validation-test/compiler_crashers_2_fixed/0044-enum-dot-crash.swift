// RUN: not %target-swift-frontend %s -emit-silgen

enum X {
    init?(a: Int) {
        .p[a]
    }
}
