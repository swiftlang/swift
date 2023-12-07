// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -solver-expression-time-threshold=1 -swift-version 5

// REQUIRES: OS=macosx

import Combine

enum Status {
  case up
  case down
}

protocol StatusMonitor {
  var statusPublisher: AnyPublisher<Status, Never> { get }
}

protocol UIController {}
protocol ControllerProtocol {}

class TestViewController : UIController, ControllerProtocol {
}

class OtherController {
  var innerController: (any UIController & ControllerProtocol)? = nil
}

class Test1 {
  var monitor: StatusMonitor

  var subscriptions: [AnyCancellable] = []
  var status: Status? = nil
  var statuses: [Status]? = nil

  init(monitor: StatusMonitor) {
    self.monitor = monitor
  }

  func simpleMapTest() {
    monitor.statusPublisher
           .map { $0 }
           .assign(to: \.status, on: self) // Ok
           .store(in: &subscriptions)
  }

  func transformationTest() {
    monitor.statusPublisher
           .map { _ in (0...1).map { _ in .up } }
           .assign(to: \.statuses, on: self) // Ok
           .store(in: &subscriptions)
  }
}

class FilteringTest {
  @Published var flag = false

  func test(viewController: inout OtherController) {
    _ = $flag.filter { !$0 }
             .map { _ in TestViewController() }
             .first()
             .handleEvents(receiveOutput: { _ in
               print("event")
             })
             .assign(to: \.innerController, on: viewController) // Ok
  }
}

extension Sequence {
  func sorted<T: Comparable>(by keyPath: KeyPath<Element, T>) -> [Element] {
    []
  }
}

func testCollectionUpcastWithTupleLabelErasure() {
  struct Item {}

  enum Info : Int, Hashable {
    case one = 1
  }


  func test(data: [Info: [Item]]) -> [(Info, [Item])] {
    data.map { $0 }
        .sorted(by: \.key.rawValue) // Ok
  }
}

do {
  struct URL {
    var path: String
    func appendingPathComponent(_: String) -> URL { fatalError() }
  }

  struct EntryPoint {
    var directory: URL { fatalError() }
  }

  func test(entryPoint: EntryPoint, data: [[String]]) {
    let _ = data.map { suffixes in
      let elements = ["a", "b"]
      .flatMap { dir in
        let directory = entryPoint.directory.appendingPathComponent(dir)
        return suffixes.map { suffix in
          directory.appendingPathComponent("\(suffix)")
        }
      }
      .map(\.path) // Ok

      return elements.joined(separator: ",")
    }
  }
}
