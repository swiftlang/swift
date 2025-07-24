/// This file implements algorithms for enumerating all monoid presentations
/// up to a given length.

func nextSymbol(_ s: inout Symbol, alphabet: Int) -> Bool {
  precondition(alphabet > 0)
  if s == alphabet - 1 {
    s = 0
    return true
  }
  s += 1
  return false
}

func nextWord(_ word: inout Word, alphabet: Int) -> Bool {
  var carry = true
  for i in word.indices.reversed() {
    carry = nextSymbol(&word[i], alphabet: alphabet)
    if !carry { break }
  }
  return carry
}

func nextRule(_ rule: inout Rule, alphabet: Int) -> Bool {
  if nextWord(&rule.rhs, alphabet: alphabet) {
    rule.rhs = Word(repeating: 0, count: rule.rhs.count)
    if nextWord(&rule.lhs, alphabet: alphabet) {
      rule.lhs = Word(repeating: 0, count: rule.lhs.count)
      return true
    }
  }
  return false
}

func nextPresentation(_ p: inout Presentation, alphabet: Int) -> Bool {
  precondition(!p.rules.isEmpty)
  var carry = true
  for i in p.rules.indices.reversed() {
    carry = nextRule(&p.rules[i], alphabet: alphabet)
    if !carry { break }
  }
  return carry
}

struct RuleShape {
  var lhs: Int
  var rhs: Int

  var rule: Rule {
    return Rule(lhs: Word(repeating: 0, count: lhs),
                rhs: Word(repeating: 0, count: rhs))
  }
}

struct PresentationShape {
  var rules: [RuleShape]

  var presentation: Presentation {
    return Presentation(rules: rules.map { $0.rule })
  }
}

func enumerateAll(alphabet: Int, shapes: [PresentationShape], output: Bool)
    -> [Presentation] {
  var filteredLHS = 0
  var filteredRHS = 0
  var filteredSymmetry = 0
  var total = 0

  var instances: [Presentation] = []
  var unique: Set<Presentation> = []

  let perms = allPermutations(alphabet)

  for shape in shapes {
    var p = shape.presentation
    var done = false

loop: while !done {
      defer {
        done = nextPresentation(&p, alphabet: alphabet)
      }

      total += 1

      for n in 0 ..< p.rules.count - 1 {
        if compare(p.rules[n].lhs, p.rules[n + 1].lhs, order: .shortlex) != .lessThan {
          filteredLHS += 1
          continue loop
        }
      }

      for rule in p.rules {
        if compare(rule.rhs, rule.lhs, order: .shortlex) != .lessThan {
          filteredRHS += 1
          continue loop
        }
      }

      if unique.contains(p.sorted(order: .shortlex)) {
        filteredSymmetry += 1
        continue
      }

      for perm in perms {
        let permuted = p.permuted(perm)
        unique.insert(permuted.sorted(order: .shortlex))
      }

      precondition(p == p.sorted(order: .shortlex))
      instances.append(p)
    }
  }

  if output {
    print("# Total \(total)")
    print("# Discarded lhs:\(filteredLHS),rhs:\(filteredRHS),"
          + "symmetry:\(filteredSymmetry)")
  }

  return instances
}

func ruleShapes(_ n: Int) -> [RuleShape] {
  precondition(n > 0)
  var result: [RuleShape] = []
  for i in 0 ..< n {
    let j = n - i

    // Don't generate rules with shorter left-hand side.
    if j < i { continue }

    result.append(RuleShape(lhs: j, rhs: i))
  }
  return result
}

func presentationShapes(rules: Int, ofLength n: Int) -> [PresentationShape] {
  precondition(n > 0)
  precondition(rules > 0)

  if rules == 1 {
    return ruleShapes(n).map {
      PresentationShape(rules: [$0])
    }
  }

  var result: [PresentationShape] = []
  for i in 1 ..< n {
    let next = presentationShapes(rules: rules - 1, ofLength: i)
    for x in ruleShapes(n - i) {
      for y in next {
        if x.lhs <= y.rules.first!.lhs {
          result.append(PresentationShape(rules: [x] + y.rules))
        }
      }
    }
  }
  return result
}

func presentationShapes(rules: Int, upToLength n: Int) -> [PresentationShape] {
  var shapes: [PresentationShape] = []
  for i in 1 ... n {
    shapes.append(contentsOf: presentationShapes(rules: rules, ofLength: i))
  }
  return shapes
}
