// RUN: %target-typecheck-verify-swift

func example(x: Int, regions: [Int]) {
  let matchingRegion =
    regions.first { region in x + region.bogusProperty }
    // expected-error@-1 {{value of type 'Int' has no member 'bogusProperty'}}
}

