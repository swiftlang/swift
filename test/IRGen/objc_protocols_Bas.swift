// RUN: %swift -parse-as-library -parse %s

// Used by the objc_protocols test to test extensions that add conformances
// using existing methods on a class.

class Bas {
  func runce() {}
}
