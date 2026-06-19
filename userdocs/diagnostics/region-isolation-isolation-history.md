# Region isolation history (RegionIsolationIsolationHistory)

## Overview

The region-based isolation checker tracks values using "regions", which are conservative approximations of object graphs. When a value is sent across an isolation boundary and that send risks a data race, the primary diagnostic explains *what* was sent and *where*. The isolation-history notes in this group explain *why* the sent value is considered isolated to a particular concurrency domain — that is, how the value came to be considered to be in the same object graph as a value that is isolated to said concurrency domain.

A typical isolation-history explanation is a chain of "X is connected to Y" notes, anchored at each named intermediate's declaration, terminating in an "originating" note at the merge that brought the sent value into an isolated region. These notes make it possible to reconstruct the merge history by following the chain backward from the value at the point of send.

We say "connected" because the region-based isolation checker reasons over regions, which are a conservative approximation of an object graph: when two values share a region, the checker has concluded that the program might construct an object-graph path between them implying that their object graphs are "connected".

Isolation-history notes are opt-in and are enabled by applying the `@diagnose(RegionIsolationIsolationHistory, as: warning)` attribute to the function. This opts that single function in to history-note emission.

When the opt-in is not active, only the primary `SendingRisksDataRace` (or related) diagnostic is emitted; the auxiliary history notes are suppressed.
