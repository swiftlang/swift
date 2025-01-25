// RUN: %target-build-swift %s -cxx-interoperability-mode=default -target arm64-apple-macos11.0

// REQUIRES: OS=macosx

import Cxx

public func takesCxxType(_ s: some CxxSequence) {}
