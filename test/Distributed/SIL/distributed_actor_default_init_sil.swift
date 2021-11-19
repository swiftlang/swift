// RUN: %target-swift-frontend -module-name test -primary-file %s -emit-sil -enable-experimental-distributed -disable-availability-checking | %FileCheck %s --enable-var-scope --dump-input=fail --implicit-check-not=actorReady --implicit-check-not=resignIdentity --implicit-check-not=hop_to_executor
// REQUIRES: concurrency
// REQUIRES: distributed

/// The convention in this test is that the Swift declaration comes before its FileCheck lines.

import _Distributed

/// Use the existential wrapper as the default actor transport.
//typealias DefaultActorTransport = AnyActorTransport


distributed actor MyDistActor {
  typealias Transport = AnyActorTransport
}
