// RUN: %target-typecheck-verify-swift

// Tests that, when synthesizing init(from:), we don't accidentally confuse
// static and instance properties with the same name. (SR-10045)
// The test fails if this file produces errors.

struct X: Codable {
  // The static property is a let with an initial value; Codable synthesis skips
  // instance properties that look like this.
  static let a: String = "a"
  
  // The instance property has no initial value, so the definite initialization
  // checker will reject an init(from:) that doesn't decode a value for it.
  let a: String
}
