# Performance Hints (PerformanceHints)

## Overview

The `PerformanceHints` diagnostic group provides opt-in guidance for identifying
language constructs and code patterns that carry non-obvious runtime performance
costs. The diagnostics are intended to help developers writing
performance-critical code make informed decisions about the abstractions they
use and to surface hidden costs that may not be apparent from the source code
alone to a non-expert Swift programmer.

This diagnostic group is **off by default**. It can be enabled as warnings with
`-Wwarning PerformanceHints`, or escalated to errors with `-Werror
PerformanceHints`.

## Sub-groups

The `PerformanceHints` group contains the following sub-groups, each of which
can also be enabled or controlled independently:

- <doc:existential-type>: Warns on use of existential types (`any Protocol`),
  which incur heap allocation, reference counting, exclusivity enforcement, and
  dynamic dispatch overhead.

- <doc:return-type-implicit-copy>: Warns when functions or closures return
  `Array` or `Dictionary` values, which leads to implicit heap-allocated copies
  of the collections.

- <doc:untyped-throws>: Warns on untyped `throws` declarations, where the use of
  `any Error` incurs a heap allocation on each `throw`.

For example, to enable only the `UntypedThrows` checks as warnings:

```
-Wwarning UntypedThrows
```

Or to enable all performance hints as errors but keep `UntypedThrows` as a
warning:

```
-Werror PerformanceHints -Wwarning UntypedThrows
```
