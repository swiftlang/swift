/// This file implements Knuth-Bendix completion and the normal form algorithm.

enum RewritingError: Error {
  case tooManyRounds
  case tooManyRules
  case tooManyNodes
  case ruleTooLong
  case tooManySteps
  case reducedWordTooLong
}

let debug = false

func log(_ str: @autoclosure () -> String) {
    if debug {
        print(str())
    }
}

struct RewritingSystem {
  var state: State = .initial

  enum State {
    case initial
    case complete
    case failed
  }

  var alphabet: Int
  var rules: [Rule] = []
  var trie: Trie

  // Limits for completion
  struct Limits: Hashable {
    var maxRounds = 100
    var maxRules = 180
    var maxLength = 100
    var maxReductionLength = 100
    var maxReductionSteps = 1 << 24
  }

  var limits: Limits

  var checkedRulesUpTo = 0  // Completion progress
  var reducedRules: [UInt32] = []  // Bitmap of reduced rules

  typealias CriticalPair = (i: Int, from: Int, j: Int)
  var criticalPairs: [CriticalPair] = []  // Temporary array for completion

  var stats = Stats()

  struct Stats {
    var numRounds = 0
    var numRulesRemaining = 0  // Number of rules that were not reduced away
    var numReductionSteps = 0
  }

  init(alphabet: Int, limits: Limits) {
    self.alphabet = alphabet
    self.trie = Trie(alphabet: self.alphabet)
    self.limits = limits

    criticalPairs.reserveCapacity(128)
  }

  mutating func addRules(_ rules: [Rule], order: Order)
      throws(RewritingError) {
    for var rule in rules {
      _ = try addRule(&rule, order: order)
    }
  }

  func reduceOne(_ word: Word, excluding: Int? = nil) -> (Int, Int)? {
    var from = 0

    while from < word.count {
      if let n = trie.lookup(word, from) {
        if n != excluding { return (from, n) }
      }

      from += 1
    }

    return nil
  }

  func reduce(_ word: inout Word, stats: inout Stats) throws(RewritingError) {
    var count = 0

    repeat {
      guard let (from, n) = reduceOne(word) else { return }

      let index = word.startIndex + from
      word.replaceSubrange(index ..< index + rules[n].lhs.count,
                           with: rules[n].rhs)
      stats.numReductionSteps += (from + rules[n].lhs.count)
      if stats.numReductionSteps > limits.maxReductionSteps { throw .tooManySteps }

      if count > limits.maxReductionLength { throw .tooManySteps }

      // FIXME: Load bearing
      if word.count > limits.maxLength { throw .reducedWordTooLong }

      count += 1
    } while true
  }

  mutating func addOrientedRule(_ rule: Rule) throws(RewritingError) {
    let longestSide = max(rule.lhs.count, rule.rhs.count)
    if longestSide > limits.maxLength { throw .ruleTooLong }

    if stats.numRulesRemaining == limits.maxRules { throw .tooManyRules }

    log("Adding rule \(rules.count) = \(rule)")
    try trie.insert(rule.lhs, rules.count)

    rules.append(rule)
    stats.numRulesRemaining += 1
  }

  mutating func addRule(_ rule: inout Rule, order: Order)
      throws(RewritingError) -> Bool {
    try reduce(&rule.lhs, stats: &stats)
    try reduce(&rule.rhs, stats: &stats)

    switch compare(rule.lhs, rule.rhs, order: order) {
    case .equal:
      return false

    case .lessThan:
      swap(&rule.lhs, &rule.rhs)
      fallthrough

    case .greaterThan:
      try addOrientedRule(rule)
      return true
    }
  }

  mutating func resolveOverlap(i: Int, from: Int, j: Int, order: Order)
      throws(RewritingError) -> Bool {
    let lhs = rules[i]
    let rhs = rules[j]

    log("Critical pair: \(i) vs \(j) at \(from)")
    log("\(printWord(rules[i].lhs))")
    log("\(String(repeating: " ", count: from))\(printWord(rules[j].lhs))")

    var rule = Rule(lhs: [], rhs: [])

    let end = lhs.lhs.count
    if from + rhs.lhs.count < end {
      rule.lhs = lhs.rhs

      rule.rhs.reserveCapacity(lhs.lhs.count - rhs.lhs.count + rhs.rhs.count)

      rule.rhs.append(contentsOf: lhs.lhs[0 ..< from])
      rule.rhs.append(contentsOf: rhs.rhs)
      rule.rhs.append(contentsOf: lhs.lhs[(from + rhs.lhs.count)...])
    } else {
      rule.lhs.reserveCapacity(lhs.rhs.count + rhs.lhs.count - lhs.lhs.count + from)
      rule.lhs.append(contentsOf: lhs.rhs)
      rule.lhs.append(contentsOf: rhs.lhs[(lhs.lhs.count - from)...])

      rule.rhs.reserveCapacity(from + rhs.rhs.count)
      rule.rhs.append(contentsOf: lhs.lhs[..<from])
      rule.rhs.append(contentsOf: rhs.rhs)
    }

    return try addRule(&rule, order: order)
  }

  mutating func processRule(_ i: Int) {
    if isReduced(i) { return }

    let lhs = rules[i]
    var from = 0
    while from < lhs.lhs.count {
      trie.visitOverlaps(lhs.lhs, from) { j in
        precondition(!isReduced(j))

        if i < checkedRulesUpTo && j < checkedRulesUpTo { return }

        if from == 0 {
          if i == j { return }
          if rules[j].lhs.count > lhs.lhs.count { return }
        }

        criticalPairs.append((i: i, from: from, j: j))
      }

      from += 1
    }
  }

  mutating func completeOne(order: Order) throws(RewritingError) -> Bool {
    precondition(state == .initial)

    precondition(criticalPairs.isEmpty)

    for i in rules.indices {
      processRule(i)
    }

    checkedRulesUpTo = rules.count
    stats.numRounds += 1

    reduceLeft()

    var confluent = true

    do {
      log("Resolving critical pairs...")
      for (i, from, j) in criticalPairs {
        if try resolveOverlap(i: i, from: from, j: j, order: order) {
          confluent = false
        }
      }
      criticalPairs.removeAll(keepingCapacity: true)
      log("All critical pairs resolved")

      try reduceRight()
    } catch let e {
      state = .failed
      throw e
    }

    if confluent {
      state = .complete
      return true
    }

    if stats.numRounds > limits.maxRounds {
      state = .failed
      throw .tooManyRounds
    }

    return false
  }

  mutating func complete(order: Order) throws(RewritingError) {
    while try !completeOne(order: order) {}
  }

  func isReduced(_ rule: Int) -> Bool {
    let i = (rule >> 5)
    let j = (rule & 31)
    if i >= reducedRules.count { return false }
    return (reducedRules[i] & (1 << j)) != 0
  }

  mutating func setReduced(_ rule: Int) {
    let i = (rule >> 5)
    let j = (rule & 31)
    while i >= reducedRules.count { reducedRules.append(0) }
    reducedRules[i] |= (1 << j)
  }

  mutating func reduceLeft() {
    if rules.isEmpty { return }
    log("Reducing left-hand sides...")
    for (n, rule) in rules.enumerated() {
      if !isReduced(n) && reduceOne(rule.lhs, excluding: n) != nil {
        log("Reduced \(n) = \(rule)")
        setReduced(n)
        trie.remove(rule.lhs, n)
        stats.numRulesRemaining -= 1
        continue
      }
    }

    precondition(stats.numRulesRemaining > 0)
  }

  mutating func reduceRight() throws(RewritingError) {
    for n in rules.indices {
      if !isReduced(n) {
        try reduce(&rules[n].rhs, stats: &stats)
      }
    }
  }

  /// Returns a complete presentation once the rewriting system is complete.
  var presentation: Presentation {
    var result: [Rule] = []
    for (n, rule) in rules.enumerated() {
      if !isReduced(n) {
        result.append(rule)
      }
    }
    return Presentation(rules: result)
  }
}
