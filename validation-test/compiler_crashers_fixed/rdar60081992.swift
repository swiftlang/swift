// RUN: %target-swift-frontend -typecheck %s

struct Model<E> where E: Comparable {
  enum TimePeriod: CaseIterable {
    case day, week, month, year
  }
}

struct MyStruct<E> where E: Comparable {
  init<EA>(entries: EA) where EA: BidirectionalCollection, EA.Element == E, EA.Index == Int {
    typealias MDict<T> = [Model<E>.TimePeriod : T] where T: Numeric
    func maxDict<T>(_ keyPath: KeyPath<Model<E>, MDict<T>>) -> MDict<T> {
      fatalError()
    }

    fatalError()
  }
}
