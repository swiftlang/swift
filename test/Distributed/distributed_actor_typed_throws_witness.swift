// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -I %t 2>&1 %s
// RUN: %target-swift-frontend -emit-silgen -target %target-swift-6.2-abi-triple -disable-availability-checking -I %t %s -D VALID_ONLY -o /dev/null

// REQUIRES: concurrency
// REQUIRES: distributed

// A remote call can fail with any error, so a distributed method can only
// witness a requirement of a protocol which does not refine 'DistributedActor'
// if the requirement throws 'any Error', not a typed error. Accepting such a
// witness used to crash SILGen when emitting the witness thunk

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

struct Mail: Codable {}
struct MyError: Error {}

protocol UntypedMailman {
  func deliver(_ mail: Mail) async throws
}

protocol AnyErrorMailman {
  func deliver(_ mail: Mail) async throws(any Error)
}

distributed actor Untyped: UntypedMailman {
  distributed func deliver(_ mail: Mail) {}
}

distributed actor AnyError: AnyErrorMailman {
  distributed func deliver(_ mail: Mail) {}
}

#if !VALID_ONLY
protocol NeverMailman {
  func deliver(_ mail: Mail) async throws(Never)
}

protocol MainActorNeverMailman {
  @MainActor func deliver(_ mail: Mail) async throws(Never)
}

protocol TypedMailman {
  func deliver(_ mail: Mail) async throws(MyError)
}

distributed actor NeverImpl: NeverMailman {
  distributed func deliver(_ mail: Mail) {}
  // expected-error@-1{{distributed instance method 'deliver' cannot satisfy requirement 'deliver' which throws 'Never'; a distributed method can throw any error}}
}

distributed actor MainActorNeverImpl: MainActorNeverMailman {
  distributed func deliver(_ mail: Mail) {}
  // expected-error@-1{{distributed instance method 'deliver' cannot satisfy requirement 'deliver' which throws 'Never'; a distributed method can throw any error}}
}

distributed actor TypedImpl: TypedMailman {
  distributed func deliver(_ mail: Mail) {}
  // expected-error@-1{{distributed instance method 'deliver' cannot satisfy requirement 'deliver' which throws 'MyError'; a distributed method can throw any error}}
}
#endif
