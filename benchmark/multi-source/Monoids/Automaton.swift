/// This file implements an algorithm to compute the number of elements in a
/// monoid (or determine it is infinite), given a complete presentation.

/// A finite state automaton, given by a set of vertices and edges.
struct Automaton {
  var states: [Word] = []
  var transitions: [(Word, Symbol, Word)] = []
}

extension Automaton {
  var hasStar: Bool {
    for state in states {
      var visited = Set<Word>()

      func rec(_ state: Word) -> Bool {
        for (from, _, to) in transitions {
          if from == state {
            if visited.contains(to) {
              return true
            } else {
              visited.insert(to)
              if rec(to) { return true }
              visited.remove(to)
            }
          }
        }

        return false
      }

      visited.insert(state)
      if rec(state) { return true }
      visited.remove(state)
    }

    return false
  }

  /// If this automaton is star-free, count the number of unique words accepted.
  var countWords: Int {
    func R(_ q: Word) -> [Word] {
      var result: [Word] = []

      for (from, _, to) in transitions {
        if to == q {
          result.append(from)
        }
      }

      return result
    }

    func T(_ q: Word, _ p: Word) -> Int {
      var letters = Set<Symbol>()
      for (from, x, to) in transitions {
        if from == q && to == p {
          letters.insert(x)
        }
      }
      return letters.count
    }

    func N(_ q: Word) -> Int {
      if q == [] {
        return 1
      }

      var result = 0
      for p in R(q) {
        result += N(p) * T(p, q)
      }
      return result
    }

    var result = 0

    for q in states {
      result += N(q)
    }

    return result
  }
}

/// Constructs an automaton to recognize the complement of this regular set:
///
///   .*(x1|x2|...).*
///
/// where 'words' is [x1, x2, ...].
///
/// This is Lemma 2.1.3 in:
///
/// String Rewriting Systems, R.V. Book, F. Otto 1993. Springer New York.
func buildAutomaton(_ words: [Word], _ alphabet: Int) -> Automaton {
  // Proper prefixes of each word.
  var prefixes = Set<Word>()

  var result = Automaton()

  func isIrreducible(_ word: Word) -> Bool {
    for i in 0 ..< word.count {
      for other in words {
        if i + other.count <= word.count {
          if Word(word[i ..< (i + other.count)]) == other {
            return false
          }
        }
      }
    }

    return true
  }

  prefixes.insert([])
  for word in words {
    for i in 0 ..< word.count {
      let prefix = Word(word[0 ..< i])
      prefixes.insert(prefix)
    }
  }

  result.states = prefixes.sorted { compare($0, $1, order: .shortlex) == .lessThan }

  for prefix in prefixes {
    for x in 0 ..< UInt8(alphabet) {
      let word = prefix + [x]

      if prefixes.contains(word) {
        result.transitions.append((prefix, x, word))
        continue
      }

      if !isIrreducible(word) {
        continue
      }

      for i in 1 ... word.count {
        let suffix = Word(word[i...])

        if prefixes.contains(suffix) {
          result.transitions.append((prefix, x, suffix))
          break
        }
      }
    }
  }

  return result
}

extension Presentation {
  /// The Irr(R) automaton.
  func automaton(alphabet: Int) -> Automaton {
    return buildAutomaton(rules.map { $0.lhs }, alphabet)
  }

  /// Returns the number of irreducible words in this monoid presentation, or
  /// nil if this set is infinite.
  ///
  /// If the presentation is complete, this is the cardinality of the
  /// presented monoid. Otherwise, it is an upper bound.
  func cardinality(alphabet: Int) -> Int? {
    let automaton = automaton(alphabet: alphabet)
    if automaton.hasStar {
      return nil
    }
    return automaton.countWords
  }
}
