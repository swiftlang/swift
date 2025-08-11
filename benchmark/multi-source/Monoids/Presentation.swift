/// This file defines the data types we use for words, rules, and
/// monoid presentations. Also, a few other fundamental algorithms
/// for working with permutations and reduction orders.

typealias Symbol = UInt8

let symbols: [Character] = ["a", "b", "c"]

func printSymbol(_ s: Symbol) -> Character {
  return symbols[Int(s)]
}

func parseSymbol(_ c: Character) -> Symbol {
  return Symbol(symbols.firstIndex(of: c)!)
}

typealias Word = [Symbol]

func printWord(_ word: Word) -> String {
  if word.isEmpty { return "1" }
  return String(word.map { printSymbol($0) })
}

func parseWord(_ str: String) -> Word {
  if str == "" || str == "1" { return [] }
  return str.map { parseSymbol($0) }
}

struct Rule: Hashable {
  var lhs: Word
  var rhs: Word
}

extension Rule: ExpressibleByStringLiteral, CustomStringConvertible {
  init(_ str: String) {
    let pair = str.split(separator: "=")
    precondition(pair.count == 2)
    self.lhs = parseWord(String(pair[0]))
    self.rhs = parseWord(String(pair[1]))
  }

  init(stringLiteral: String) {
    self.init(stringLiteral)
  }

  var description: String {
    return "\(printWord(lhs))=\(printWord(rhs))"
  }
}

struct Presentation: Hashable {
  var rules: [Rule]

  var alphabet: Int {
    var result: Int = 0
    for rule in rules {
      for s in rule.lhs { result = max(result, Int(s)) }
      for s in rule.rhs { result = max(result, Int(s)) }
    }
    return result + 1
  }

  var longestRule: Int {
    var result = 0
    for rule in rules {
      result = max(result, rule.lhs.count)
      result = max(result, rule.rhs.count)
    }
    return result
  }
}

extension Presentation: ExpressibleByStringLiteral, CustomStringConvertible {
  init(_ str: String) {
    self.rules = str.split(separator: ",").map { Rule(String($0)) }
  }

  init(stringLiteral: String) {
    self.init(stringLiteral)
  }

  var description: String {
    if rules.isEmpty { return "," }
    return rules.map { $0.description }.joined(separator: ",")
  }
}

typealias Permutation = [Int]

func identityPermutation(_ n: Int) -> Permutation {
  return Permutation(0 ..< n)
}

func inversePermutation(_ perm: Permutation) -> Permutation {
  var result = Permutation(repeating: 0, count: perm.count)
  for (i, j) in perm.enumerated() {
    result[j] = i
  }
  return result
}

// TAOCP 4A 7.2.1.2 Algorithm L
func nextPermutation(_ perm: inout Permutation) -> Bool {
  var j = perm.count - 2
  while j >= 0 && perm[j] >= perm[j + 1] {
    j -= 1
  }
  if j < 0 { return true }

  var l = perm.count - 1
  while perm[j] >= perm[l] { l -= 1 }
  perm.swapAt(l, j)

  l = perm.count - 1
  var k = j + 1
  while k < l {
    perm.swapAt(k, l)
    k += 1
    l -= 1
  }

  return false
}

func allPermutations(_ alphabet: Int) -> [Permutation] {
  var perm = identityPermutation(alphabet)
  var perms: [Permutation] = []
  repeat {
    perms.append(perm)
  } while !nextPermutation(&perm)
  return perms
}

extension Word {
  func permuted(_ perm: Permutation) -> Word {
    return map { Symbol(perm[Int($0)]) }
  }
}

extension Rule {
  func permuted(_ perm: Permutation) -> Rule {
    return Rule(lhs: lhs.permuted(perm), rhs: rhs.permuted(perm))
  }
}

extension Presentation {
  func permuted(_ perm: Permutation) -> Presentation {
    return Presentation(rules: rules.map { $0.permuted(perm) })
  }
}

enum CompareResult {
  case lessThan
  case equal
  case greaterThan
}

enum Order: Hashable {
  case shortlex
  case permutation(Permutation)
  case wreath([Int], Permutation)

  var simplified: Order {
    switch self {
    case .shortlex:
      return self
    case .permutation(let perm):
      // shortlex with identity permutation avoids some indirection
      if perm == identityPermutation(perm.count) {
        return .shortlex
      }
      return self
    case .wreath(_, _):
      return self
    }
  }

  func removeGenerator(_ a: Symbol) -> Order {
    func updatePermutation(_ perm: Permutation, removing i: Int) -> Permutation {
      return Permutation(perm[0 ..< i] + perm[(i + 1)...]).map {
        return $0 > perm[i] ? $0 - 1 : $0
      }
    }

    switch self {
    case .shortlex:
      return self

    case .permutation(let perm):
      return .permutation(updatePermutation(perm, removing: Int(a)))

    case .wreath(let degrees, let perm):
      var newDegrees = Array(degrees[0 ..< Int(a)] + degrees[(Int(a) + 1)...])
      let oldDegree = degrees[Int(a)]
      if newDegrees.firstIndex(of: oldDegree) == nil {
        newDegrees = newDegrees.map { $0 > oldDegree ? $0 - 1 : $0 }
      }
      let newPerm = updatePermutation(perm, removing: Int(a))
      if newDegrees.max()! == 0 { return .permutation(newPerm) }
      return .wreath(newDegrees, newPerm)
    }
  }
}

func shortlex(_ lhs: Word, _ lhsFrom: Int, _ lhsTo: Int,
              _ rhs: Word, _ rhsFrom: Int, _ rhsTo: Int,
              perm: Permutation) -> CompareResult {
  let lhsCount = (lhsTo - lhsFrom)
  let rhsCount = (rhsTo - rhsFrom)
  if lhsCount != rhsCount {
    return lhsCount < rhsCount ? .lessThan : .greaterThan
  }

  for i in 0 ..< lhsCount {
    let x = lhs[lhsFrom + i]
    let y = rhs[rhsFrom + i]
    if x != y {
      return perm[Int(x)] < perm[Int(y)] ? .lessThan : .greaterThan
    }
  }

  return .equal
}

// The "wreath product" or "recursive path" order:
//
// Sims, C. C. (1994). Computation with Finitely Presented Groups.
// Cambridge: Cambridge University Press.
//
func wreath(_ lhs: Word, _ lhsFrom: Int, _ lhsTo: Int,
            _ rhs: Word, _ rhsFrom: Int, _ rhsTo: Int,
            degrees: [Int], perm: Permutation) -> CompareResult {
  var i = lhsFrom
  var j = rhsFrom

  while true {
    if i == lhsTo {
      if j == rhsTo { return .equal }
      return .lessThan
    } else if j == rhsTo {
      return .greaterThan
    }

    if lhs[i] != rhs[j] { break }
    i += 1
    j += 1
  }

  func maxDegree(_ word: Word, _ from: Int, _ to: Int)
      -> (degree: Int, count: Int, symbol: Symbol?) {
    var degree = -1, count = 0
    var symbol: Symbol? = nil

    for s in word[from ..< to] {
      if degrees[Int(s)] > degree {
        degree = degrees[Int(s)]
        count = 1
        symbol = s
      } else if degrees[Int(s)] == degree {
        count += 1
        if symbol != s { symbol = nil }
      }
    }

    return (degree, count, symbol)
  }

  let (lhsMaxDegree, lhsCount, lhsHeadSymbol) = maxDegree(lhs, i, lhsTo)
  let (rhsMaxDegree, rhsCount, rhsHeadSymbol) = maxDegree(rhs, j, rhsTo)
  if lhsMaxDegree < rhsMaxDegree {
    return .lessThan
  } else if lhsMaxDegree > rhsMaxDegree {
    return .greaterThan
  } else if lhsCount < rhsCount {
    return .lessThan
  } else if lhsCount > rhsCount {
    return .greaterThan
  }

  if lhsHeadSymbol != nil && rhsHeadSymbol != nil {
    if lhsHeadSymbol != rhsHeadSymbol {
      return perm[Int(lhsHeadSymbol!)] < perm[Int(rhsHeadSymbol!)]
          ? .lessThan : .greaterThan
    }
  } else {
    if lhsMaxDegree == 0 {
      return shortlex(lhs, i, lhsTo, rhs, j, rhsTo, perm: perm)
    } else {
      let lhsHeadWord = lhs[i ..< lhsTo].filter { degrees[Int($0)] == lhsMaxDegree }
      let rhsHeadWord = rhs[j ..< rhsTo].filter { degrees[Int($0)] == rhsMaxDegree }

      let result = shortlex(lhsHeadWord, 0, lhsHeadWord.count,
                            rhsHeadWord, 0, rhsHeadWord.count,
                            perm: perm)
      if result != .equal { return result }
    }
  }

  if lhsMaxDegree == 0 { return .equal }

  var ii = i, jj = j
  while i < lhsTo {
    while i < lhsTo && degrees[Int(lhs[i])] != lhsMaxDegree { i += 1 }
    while j < rhsTo && degrees[Int(rhs[j])] != rhsMaxDegree { j += 1 }

    let result = wreath(lhs, ii, i, rhs, jj, j, degrees: degrees, perm: perm)
    if result != .equal { return result }

    i += 1; ii = i
    j += 1; jj = j
  }

  precondition(j == rhsTo)
  return .equal
}

func compare(_ lhs: Word, _ rhs: Word, order: Order) -> CompareResult {
  switch order {
  case .shortlex:
    if lhs.count != rhs.count {
      return lhs.count < rhs.count ? .lessThan : .greaterThan
    }

    for i in lhs.indices {
      let x = lhs[i]
      let y = rhs[i]
      if x != y {
        return x < y ? .lessThan : .greaterThan
      }
    }

    return .equal

  case .permutation(let perm):
    return shortlex(lhs, 0, lhs.count, rhs, 0, rhs.count, perm: perm)

  case .wreath(let degrees, let perm):
    return wreath(lhs, 0, lhs.count, rhs, 0, rhs.count,
                  degrees: degrees, perm: perm)
  }
}

func compare(_ lhs: Rule, _ rhs: Rule, order: Order) -> CompareResult {
  let result = compare(lhs.lhs, rhs.lhs, order: order)
  if result != .equal {
    return result
  }

  return compare(lhs.rhs, rhs.rhs, order: order)
}

extension Rule {
  func oriented(order: Order) -> Rule? {
    switch compare(lhs, rhs, order: order) {
    case .equal:
      return nil
    case .lessThan:
      return Rule(lhs: rhs, rhs: lhs)
    case .greaterThan:
      return self
    }
  }
}

extension Presentation {
  func sorted(order: Order) -> Presentation {
    let sortedRules =
      rules.map { $0.oriented(order: order)! }
           .sorted { compare($0, $1, order: order) == .lessThan }
    return Presentation(rules: sortedRules)
  }
}
