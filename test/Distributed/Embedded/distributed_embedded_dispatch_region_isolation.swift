// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -swift-version 6 -enable-experimental-feature Embedded -parse-as-library -wmo -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

// The synthesized receive dispatcher '_executeDistributedTarget' is checked
// in the user's language mode. In Swift 6 it must not diagnose region
// isolation errors: the decoded arguments are fresh values sent to the actor,
// and the result handler (not Sendable here, its methods are '@concurrent') is
// only used sequentially to report the outcome of the call

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

final class NonSendable: EmbeddedSerializationRequirement {
  var x = 0
  init() {}
  var serializedByteCount: Int { 0 }
  func encode(into output: inout OutputSpan<UInt8>) {}
  static func decode(from input: inout Span<UInt8>) throws -> NonSendable {
    NonSendable()
  }
}

distributed actor D {
  distributed func take(_ n: NonSendable) {}
  distributed func takeTwo(_ a: NonSendable, _ b: NonSendable) {}
  distributed func echo(_ n: NonSendable) -> NonSendable { n }
  distributed func noArgs() {}
}
