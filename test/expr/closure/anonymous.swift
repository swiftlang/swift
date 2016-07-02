// RUN: %target-parse-verify-swift

func takeIntToInt(_ f: (Int) -> Int) { }
func takeIntIntToInt(_ f: (Int, Int) -> Int) { }

// Simple closures with anonymous arguments
func simple() {
  takeIntToInt({return $0 + 1}) // expected-note {{use trailing closure to simplify arguments}}
  takeIntIntToInt({return $0 + $1 + 1}) // expected-note {{use trailing closure to simplify arguments}}
}

