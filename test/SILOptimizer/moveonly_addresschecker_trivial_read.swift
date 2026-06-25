// RUN: %target-swift-frontend -emit-sil -verify %s
// RUN: %target-swift-frontend -emit-sil -verify -enable-sil-opaque-values %s

func use(_: Int32) {}

struct NoncopyableYieldingSubscript: ~Copyable {
    subscript() -> Int16 {
        _read {
            yield 0
        }
    }
}

func foo(component: inout NoncopyableYieldingSubscript, condition: Bool) {
    let extracted: Int32

    switch condition {
    case true:
        extracted = Int32(component[])

    case false:
        extracted = Int32(component[])

    }

    use(extracted)
}
