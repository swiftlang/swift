public protocol Problem : class {
}

public extension Problem {
}

public func callGenericWitness<T: Problem> (_ t: T) async -> Int {
  return 0
}
