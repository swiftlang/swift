// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -swift-version 6 -emit-sil -o /dev/null -verify %s
// REQUIRES: concurrency
// expected-no-diagnostics

@MainActor class SomeMainActor {
    var task: Task<Void, Error>?
    func start() {
        task = Task {}
    }
}
