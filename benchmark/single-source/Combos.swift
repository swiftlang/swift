// Combos benchmark
//
// Description: Generates every combination of every element in two sequences
// Source: https://gist.github.com/airspeedswift/34251f2ddb1b038c5d88

import TestsUtils

public var Combos = BenchmarkInfo(
  name: "Combos",
  runFunction: run_Combos,
  tags: [.validation, .abstraction]
)

@inline(never)
public func run_Combos(_ N: Int) {
  let firstRef = [Character("A"), Character("0")]
  let lastRef = [Character("J"), Character("9")]
  var combos = [[Character]]()

  for _ in 1...10*N {
    combos = combinations("ABCDEFGHIJ", "0123456789").map {
      return [$0] + [$1]
    }

    if combos.first! != firstRef || combos.last! != lastRef {
      break
    }
  }

  CheckResults(combos.first! == firstRef && combos.last! == lastRef)
}

func combinations
<First: Sequence, Second: Collection>
(_ first: First, _ second: Second)
-> AnyIterator<(First.Element, Second.Element)> {
  var first_gen = first.makeIterator()
  var second_gen = second.makeIterator()
  var current_first = first_gen.next()
  return AnyIterator {
    // check if there's more of first to consume
    if let this_first = current_first {
      // if so, is there more of this go-around
      // of second to consume?
      if let this_second = second_gen.next() {
        return (this_first, this_second)
      }
      // if second used up, reset it
      else {
        // go back to the beginning of second
        second_gen = second.makeIterator()
        // and take the first element of it
        let next_second = second_gen.next()
        // was there a first element?
        if let this_second = next_second {
          // move on to the next element of first
          current_first = first_gen.next()
          // is there such an element?
          if let this_first = current_first {
            return (this_first, this_second)
          }
          // if not, we've reached the end of
          // the first sequence
          else {
            // so we've finished
            return nil
          }
        }
        // if not, second is empty
        else {
          // so we need to guard against
          // infinite looping in that case
          return nil
        }
      }
    }
    // if not, we've reached the end of
    // the first sequence
    else {
      // so we've finished
      return nil
    }
  }
}
