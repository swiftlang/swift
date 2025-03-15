// RUN: %batch-code-completion

// NONISOLATED_UNSAFE: Keyword/None:                       unsafe; name=unsafe

nonisolated(#^NONISOLATED_UNSAFE_TOP_LEVEL?check=NONISOLATED_UNSAFE^#) var count = 0

struct MyStruct {
  nonisolated(#^NONISOLATED_UNSAFE_IN_STRUCT?check=NONISOLATED_UNSAFE^#) var prop = 0
}
