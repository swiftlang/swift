// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5 -strict-concurrency=complete

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

@MainActor
class Visibility: ObservableObject {
    nonisolated init() {}

    @Published var yes = false // some nonsense
}

struct CoffeeTrackerView: View {
    nonisolated init() {}

    @ObservedObject var showDrinkList: Visibility = Visibility()

    var storage: Visibility = Visibility()

    var body: some View {
        VStack {
          Button(action: {}) {
            Image(self.showDrinkList.yes ? "add-coffee" : "add-tea")
                    .renderingMode(.template)
            }
        }
    }
}

@MainActor
func fromMainActor() async {
  let view = CoffeeTrackerView()
  _ = view.body
  _ = view.showDrinkList
  _ = view.storage
}


func fromConcurrencyAware() async {
  let view = CoffeeTrackerView() // synthesized 'init' is 'nonisolated'

  // expected-note@+3 {{property access is 'async'}}
  // expected-warning@+2 {{non-sendable type 'some View' of property 'body' cannot exit main actor-isolated context}}
  // expected-warning@+1 {{expression is 'async' but is not marked with 'await'}}
  _ = view.body

  // expected-note@+2 {{property access is 'async'}}
  // expected-warning@+1 {{expression is 'async' but is not marked with 'await'}}
  _ = view.showDrinkList

  _ = view.storage
}
