// RUN: %swift -c %s -cxx-interoperability-mode=default -target arm64-apple-ios7.0-simulator

// REQUIRES: DARWIN_SIMULATOR=ios

import Cxx

public func takesCxxType(_ s: some CxxSequence) {}
