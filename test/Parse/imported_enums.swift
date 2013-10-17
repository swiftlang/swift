// RUN: %swift -parse %s

// This is used by the matching_patterns test to test qualified reference to
// enum elements.

enum ImportedEnum {
  case Simple
  case Compound(Int)
}
