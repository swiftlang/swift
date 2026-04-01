/// Generate a bunch of reduction orders.
func getExhaustiveOrderMix(_ alphabet: Int, _ extraAlphabet: Int) -> [Order] {
  var result: [Order] = []

  let permutations = allPermutations(alphabet + extraAlphabet)

  for perm in permutations {
    result.append(.permutation(perm))
  }

  for perm in permutations {
    let degrees = perm.map { Int($0) }
    result.append(.wreath(degrees, perm))
  }

  return result
}

/// Parameters for completion.
struct Strategy: Sendable {
  var extra: [Rule] = []
  var factorLength: Int? = nil
  var frequency: Int = 0
  var order: Order = .shortlex

  func withOrder(_ order: Order) -> Self {
    var result = self
    result.order = order
    return result
  }
}

extension Presentation {
  func collectFactors(order: Order, upToLength: Int) -> [Word] {
    let strategy = Strategy()
    var rws = RewritingSystem(alphabet: alphabet)

    do {
      try rws.addRules(rules, order: strategy.order)

      for _ in [0, 1] {
        if try rws.completeOne(order: strategy.order) { break }
      }
    } catch {}

    return rws.collectFactors(upToLength: upToLength, order: strategy.order)
  }
}

extension Word {
  func collectFactors(_ table: inout [Word: Int], length: Int) {
    precondition(length >= 2)

    if length > count { return }

    for i in 0 ... count - length {
      table[Word(self[i ..< i + length]), default: 0] += 1
    }
  }
}

extension RewritingSystem {
  func collectFactors(upToLength: Int, order: Order) -> [Word] {
    var table: [Word: Int] = [:]
    for n in rules.indices {
      if isReduced(n) { continue }
      let lhs = rules[n].lhs
      for length in 2 ... upToLength {
        lhs.collectFactors(&table, length: length)
      }
    }

    return table.sorted {
      $0.1 > $1.1 || ($0.1 == $1.1 &&
                      compare($0.0, $1.0, order: order) == .greaterThan)
    }.map { $0.key }
  }
}

struct Solution {
  let extra: [Rule]
  let cardinality: Int?
  let presentation: Presentation
}

extension RewritingSystem {
  func formSolution(_ strategy: Strategy) -> Solution {
    let p = presentation.sorted(order: strategy.order)
    return Solution(extra: strategy.extra,
                    cardinality: cardinality,
                    presentation: p)
  }
}

extension Presentation {
  func complete(_ strategy: Strategy) throws(RewritingError) -> Solution {
    var rws = RewritingSystem(alphabet: alphabet + strategy.extra.count)
    try rws.addRules(rules, order: strategy.order)
    try rws.addRules(strategy.extra, order: strategy.order)

    try rws.complete(order: strategy.order)
    return rws.formSolution(strategy)
  }
}
