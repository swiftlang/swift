// RUN: %swift -parse %s

// This is used by the matching_patterns test to test qualified reference to
// union elements.

union ImportedUnion {
  case Simple
  case Compound(Int)
}
