// RUN: %target-swift-frontend %s -sil-verify-all -c -o /dev/null

// This file is suppose to contain programs that are "correct" through emitting
// an object file, so no diagnostic tests in here, please!

// rdar://118274699
struct Env: ~Copyable {
    var constants: Int
}
struct VM: ~Copyable {
    let env = Env(constants: 0)

    mutating func run() {
        env.constants
    }
}