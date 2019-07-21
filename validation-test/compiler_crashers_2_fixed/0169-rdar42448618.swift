// RUN: not %target-swift-frontend -emit-ir %s

protocol ObservableType {
  associatedtype E
}

extension ObservableType where E == Any {
  static func zip<O1>(_ source1: O1) { fatalError() }
}

extension ObservableType {
  static func zip<O1, O2>(_ source1: O1, _ source2: O2) { fatalError() }
}

class Observable<Element> : ObservableType {
  public typealias E = Element
}

Observable.zip(1, 2)
