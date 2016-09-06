// RUN: %target-parse-verify-swift %s -enable-astscope-lookup

// Name binding in default arguments

// FIXME: Semantic analysis should produce an error here, because 'x'
// is not actually available.
func functionParamScopes(x: Int, y: Int = x) -> Int {
  return x + y
}

