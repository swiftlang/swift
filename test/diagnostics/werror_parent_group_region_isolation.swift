// Default: the child group's diagnostic is a warning in language mode 5.
// RUN: %target-swift-frontend -emit-sil -o /dev/null %s -target %target-swift-5.1-abi-triple -strict-concurrency=complete -swift-version 5 -verify -verify-additional-prefix warnonly-

// Ensure escalated with `-Werror` on the child group.
// RUN: %target-swift-frontend -emit-sil -o /dev/null %s -target %target-swift-5.1-abi-triple -strict-concurrency=complete -swift-version 5 -Werror SendingRisksDataRace -verify -verify-additional-prefix erronly-

// Ensure escalated with `-Werror` on the parent group.
// RUN: %target-swift-frontend -emit-sil -o /dev/null %s -target %target-swift-5.1-abi-triple -strict-concurrency=complete -swift-version 5 -Werror RegionIsolation -verify -verify-additional-prefix erronly-

// REQUIRES: concurrency

class NS {}

@MainActor func take(_ x: NS) {}

func test() async {
  let x = NS()
  await take(x) // expected-warnonly-warning {{sending 'x' risks causing data races}}
  // expected-erronly-error@-1 {{sending 'x' risks causing data races}}
  // expected-note@-2 {{sending 'x' to main actor-isolated global function 'take' risks causing data races between main actor-isolated and local nonisolated uses}}
  print(x) // expected-note {{access can happen concurrently}}
}
