// RUN: %swift -parse %s

// This is used by the matching_patterns test to test qualified reference to
// oneof elements.

oneof ImportedOneof {
  case Simple
  case Compound(Int)
}
