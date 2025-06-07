// RUN: %target-typecheck-verify-swift

struct Test {
    var v: String
    var i: Int
}

do {
    let _ = Array(1 ... 20).map { i in
        _ = 0
        return Test(v: "\(i * 1000)", i: 1)
    }
}
