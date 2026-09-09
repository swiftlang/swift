// RUN: %target-swift-frontend -typecheck -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/PortableRoundtripActorSystem.swift
// RUN: %target-swift-frontend -typecheck -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/PortableRoundtripActorSystem.swift

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed

// The SAME distributed-actor source below type-checks in both Embedded and
// ordinary Swift. The mode-specific machinery (the actor system, its
// serialization layer, and the `remoteCall` family shape) all lives in the
// shared Inputs/PortableRoundtripActorSystem.swift, behind `#if $Embedded`, so
// user code never mentions a mode at all.

import _Concurrency
import Distributed

distributed actor Greeter {
  distributed func hello(name: String) -> String {
    return "Hello, \(name)!"
  }
}
