public protocol Problem : class {
  func theProblem() async -> Int
}

public extension Problem {
  func theProblem() async -> Int {
			return 1
  }
}

public func callGenericWitness<T: Problem> (_ t: T) async -> Int {
  return await t.theProblem()
}
