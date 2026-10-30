# Region-based isolation (RegionIsolation)

## Overview

The `RegionIsolation` diagnostic group contains the diagnostics emitted by the
region-based isolation checker, which prevents data races on values that don't
conform to `Sendable`.

The checker groups values into regions, which are conservative approximations
of object graphs. Two values are in the same region if the program might be
able to reach one from the other. A region is either disconnected, meaning no
concurrency domain can access it yet, or isolated to exactly one concurrency
domain: an actor, a global actor, or the current task.

A non-`Sendable` value can be sent to another concurrency domain, for example
by passing it to a function on an actor or capturing it in a task closure, as
long as nothing in the original concurrency domain can access its region after
the send. The diagnostics in this group report code that sends a value while
its region is still accessible, or that would merge regions isolated to
different concurrency domains.

In the Swift 6 language mode, these diagnostics are errors. In the Swift 5
language mode, they're reported as warnings when complete concurrency checking
is enabled with `-strict-concurrency=complete`.

## Sub-groups

The `RegionIsolation` group contains the following sub-groups, each of which
explains a specific kind of diagnostic in more detail:

- <doc:sending-risks-data-race>: A non-`Sendable` value is sent to another
  concurrency domain while the original concurrency domain can still access it.

- <doc:sending-closure-risks-data-race>: A closure sent to another concurrency
  domain captures a value that the original concurrency domain can still
  access.

- <doc:region-isolation-cross-isolation-data-race>: An operation would merge
  two regions that are isolated to different concurrency domains.

- <doc:region-isolation-isolation-history>: Opt-in notes that explain how a
  sent value came to be isolated to a particular concurrency domain.

- <doc:region-isolation-unknown-pattern>: The checker found an isolation error
  but can't describe it yet. This indicates a compiler bug.

## See Also

- [SE-0414: Region based isolation](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0414-region-based-isolation.md)
- [SE-0430: `sending` parameter and result values](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0430-transferring-parameters-and-results.md)
