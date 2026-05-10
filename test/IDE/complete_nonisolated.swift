// RUN: %batch-code-completion

// NONISOLATED-DAG: Keyword/None:                       unsafe; name=unsafe
// NONISOLATED-DAG: Keyword/None:                       nonsending; name=nonsending

nonisolated(#^NONISOLATED_UNSAFE_TOP_LEVEL?check=NONISOLATED^#) var count = 0

struct MyStruct {
  nonisolated(#^NONISOLATED_UNSAFE_IN_STRUCT?check=NONISOLATED^#) var prop = 0
}
