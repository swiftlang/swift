// RUN: %target-typecheck-verify-swift

// Test the AST Verifier assertion for the VarDecl getting its Generic
// Environment.
func variadic<each T>(ts: repeat each T) {
  for t in repeat each ts {
    func inner() {}
    let y = t
    // expected-warning@-1{{initialization of immutable value 'y' was never used; consider replacing with assignment to '_' or removing it}}
  }
}
