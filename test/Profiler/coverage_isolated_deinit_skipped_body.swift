// Make sure we don't crash when an isolated deinit body is skipped but
// coverage instrumentation is enabled.
// RUN: %target-swift-frontend -emit-module -experimental-skip-non-inlinable-function-bodies-without-types -profile-generate -profile-coverage-mapping %s

// REQUIRES: concurrency

final class Resource {
    func cleanup() {}
}

public actor MyActor {
    private let resource = Resource()

    isolated deinit {
        resource.cleanup()
    }
}
