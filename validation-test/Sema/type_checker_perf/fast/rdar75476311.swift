// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -solver-expression-time-threshold=1 -swift-version 5
// REQUIRES: tools-release,no_asan,objc_interop
// REQUIRES: OS=macosx

import Combine
import Foundation
import simd

class Entry : NSObject {}

class Query : NSObject {
}

class Test {
  typealias QueryPublisher = AnyPublisher<Query, Never>

  @Published var tokens: [Entry] = []
  @Published var text: String = ""

  var test: QueryPublisher {
    $tokens
     .combineLatest($text)
     .debounce(for: 0.2, scheduler: RunLoop.main)
     .removeDuplicates(by: { (first, second) -> Bool in
       first.0 == second.0 && first.1 == second.1 // Ok (lot of overloads here) and first `==` is different from second
     })
     .map { _ in
       Query()
     }
     .eraseToAnyPublisher()
  }
}
