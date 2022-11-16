// rdar://102405053 – Make sure we can handle skipped bodies at the top level.
// RUN: %target-swift-frontend -emit-module -experimental-skip-non-inlinable-function-bodies-without-types -profile-generate -profile-coverage-mapping %s

func foo() {}
