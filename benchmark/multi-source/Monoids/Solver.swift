/// This file implements the driver loop which attempts Knuth-Bendix completion
/// with various strategies on all instances in parallel.

struct Dispatcher {
  let subset: [Int]
  let strategies: [Strategy]
  var currentStrategy = 0
  var currentInstance = 0

  mutating func next() -> (instance: Int, strategy: Int)? {
    if subset.isEmpty || currentStrategy == strategies.count { return nil }

    defer {
      currentInstance += 1
      if currentInstance == subset.count {
        currentInstance = 0
        currentStrategy += 1
      }
    }

    return (instance: subset[currentInstance],
            strategy: currentStrategy)
  }
}

struct Solver {
  let alphabet: Int
  let instances: [Presentation]

  // This is a list of indices of all remaining unsolved instances.
  var subset: [Int]

  var factors: [Order: [Int: [Word]]] = [:]
  var maxFactors: [Int] = []

  var output: Bool

  init(alphabet: Int, instances: [Presentation], output: Bool) {
    self.alphabet = alphabet
    self.instances = instances
    self.subset = Array(instances.indices)
    self.output = output
  }

  mutating func solve() async {
    if output {
      print("# Remaining \(subset.count)")
      print("# n:\tpresentation:\tcardinality:\tcomplete presentation:\tstrategy:")
    }

    // The shortlex order with identity permutation of generators solves
    // almost everything.
    await attempt([Strategy()])

    if output {
      print("# Remaining \(subset.count)")
      print("# Attempting more reduction orders")
    }

    var orderMix: [Int: [Order]] = [:]
    for i in [0, 1] {
      orderMix[alphabet + i] = getExhaustiveOrderMix(alphabet, i)
    }

    do {
      var strategies: [Strategy] = []

      let strategy = Strategy()
      let orders = orderMix[alphabet]!

      // We already did the first one.
      for order in orders[1...] {
        strategies.append(strategy.withOrder(order))
      }

      await attempt(strategies)
    }

    if output {
      print("# Remaining \(subset.count)")
      print("# Attempting to add a generator")
    }

    do {
      collectFactors(orderMix[alphabet]!)

      var strategies: [Strategy] = []

      for frequency in [0, 1] {
        for factorLength in 2 ..< maxFactors.count {
          let strategy = Strategy(factorLength: factorLength,
                                  frequency: frequency)
          for order in orderMix[alphabet + 1]! {
            strategies.append(strategy.withOrder(order))
          }
        }
      }

      await attempt(strategies)
    }

    if output {
      print("# Remaining \(subset.count)")

      for n in subset {
        let instance = instances[n]
        print("\(n)\t\(instance)\thard")
      }
    }
  }

  mutating func collectFactors(_ orders: [Order]) {
    for order in orders {
      for n in subset {
        factors[order, default: [:]][n] =
            collectFactors(n, instances[n], order: order.simplified)
      }
    }
  }

  mutating func collectFactors(_ n: Int, _ p: Presentation, order: Order) -> [Word] {
    // FIXME: The 2 is a magic number
    let words = p.collectFactors(order: order, upToLength: p.longestRule + 2)
    if !words.isEmpty {
      let longestFactor = words.map { $0.count }.max()!
      for i in 2 ... longestFactor {
        let factorsOfLength = words.filter { $0.count == i }
        while maxFactors.count <= i {
          maxFactors.append(0)
        }
        maxFactors[i] = max(maxFactors[i], factorsOfLength.count)
      }
    }
    return words
  }

  func prepare(_ instance: Int, _ strategy: Strategy) -> Strategy? {
    var strategy = strategy

    var factorsOfLength: [Word] = []
    if let length = strategy.factorLength {
      let order = strategy.order.removeGenerator(Symbol(alphabet))
      factorsOfLength = (factors[order]!)[instance]!

      let factorsOfLength = factorsOfLength.filter { $0.count == length }
      if strategy.frequency >= factorsOfLength.count { return nil }

      // Add a new generator 'c' and a rule 'c=x' for a magic factor 'x'.
      let newFactor = factorsOfLength[strategy.frequency]
      let newGenerator = [Symbol(alphabet)]

      // If 'c' is just going to reduce to 'x' there's no point in
      // considering it further.
      if compare(newFactor, newGenerator, order: strategy.order) == .lessThan {
        return nil
      }

      strategy.extra = [Rule(lhs: newFactor, rhs: newGenerator)]
    }

    strategy.order = strategy.order.simplified
    return strategy
  }

  mutating func attempt(_ strategies: [Strategy]) async {
    if subset.isEmpty { return }

    let solved = await withTaskGroup(of: (Int, Int, Solution?).self) { group in
      var dispatcher = Dispatcher(subset: subset,
                                  strategies: strategies)
      var solved: [Int: (Int, Solution)] = [:]
      var pending: [Int: Int] = [:]

      func startTask() {
        while true {
          guard let (instance, strategyIndex) = dispatcher.next() else {
            return
          }

          if solved[instance] != nil {
            continue
          }

          guard let strategy = prepare(instance, strategies[strategyIndex]) else {
            continue
          }

          pending[strategyIndex, default: 0] += 1

          let p = instances[instance]

          group.addTask { () -> (Int, Int, Solution?) in
            if let solution = try? p.complete(strategy) {
              return (instance, strategyIndex, solution)
            }
            return (instance, strategyIndex, nil)
          }

          return
        }
      }

      func completeTask(_ instance: Int, _ strategyIndex: Int, _ solution: Solution?) {
        pending[strategyIndex, default: 0] -= 1
        precondition(pending[strategyIndex]! >= 0)

        if let solution {
          // The lowest-numbered strategy is the 'official' solution for the instance.
          var betterStrategy = true
          if let (oldStrategyIndex, _) = solved[instance] {
            if oldStrategyIndex < strategyIndex {
              betterStrategy = false
            }
          }

          if betterStrategy {
            solved[instance] = (strategyIndex, solution)
          }
        }

        retireStrategies()
      }

      func retireStrategies() {
        var retired: [Int: [Int]] = [:]
        let pendingInOrder = pending.sorted { $0.key < $1.key }
        for (strategyIndex, numTasks) in pendingInOrder {
          precondition(strategyIndex <= dispatcher.currentStrategy)
          if dispatcher.currentStrategy == strategyIndex { break }
          if numTasks > 0 { break }

          pending[strategyIndex] = nil
          retired[strategyIndex] = []
        }

        if retired.isEmpty { return }

        // If we are done dispatching a strategy, look at all instances solved
        // by that strategy and print them out.
        for n in subset {
          if let (strategyIndex, solution) = solved[n] {
            if retired[strategyIndex] != nil {
              retired[strategyIndex]!.append(n)

              // Print the instance and solution.
              var str = "\(n)\t\(instances[n])\t"

              if let cardinality = solution.cardinality {
                str += "finite:\(cardinality)"
              } else {
                str += "infinite"
              }
              str += "\tfcrs:\(solution.presentation)"

              // Print the extra generators that were added, if any.
              if !solution.extra.isEmpty {
                str += "\t\(Presentation(rules: solution.extra))"
              }

              if output {
                print(str)
              }
            }
          }
        }
      }

      // We run 32 tasks at a time.
      for _ in 0 ..< 32 {
        startTask()
      }

      for await (instance, strategyIndex, solution) in group {
        startTask()
        completeTask(instance, strategyIndex, solution)
      }

      return solved
    }

    subset = subset.filter { solved[$0] == nil }
  }
}
