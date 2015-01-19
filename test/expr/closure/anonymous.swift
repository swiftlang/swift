// RUN: %target-parse-verify-swift

func takeIntToInt(f: (Int) -> Int) { }
func takeIntIntToInt(f: (Int, Int) -> Int) { }

// Simple closures with anonymous arguments
func simple() {
  takeIntToInt({return $0 + 1})
  takeIntIntToInt({return $0 + $1 + 1})
}

