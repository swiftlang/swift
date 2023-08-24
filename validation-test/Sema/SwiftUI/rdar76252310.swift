// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5 -strict-concurrency=targeted

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

class Visibility: ObservableObject { // expected-note 2{{class 'Visibility' does not conform to the 'Sendable' protocol}}
    @Published var yes = false // some nonsense
}

struct CoffeeTrackerView: View { // expected-note 4{{consider making struct 'CoffeeTrackerView' conform to the 'Sendable' protocol}}
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
  // expected-note@+3 {{calls to initializer 'init()' from outside of its actor context are implicitly asynchronous}}
  // expected-error@+2 {{expression is 'async' but is not marked with 'await'}}
  // expected-warning@+1 {{non-sendable type 'CoffeeTrackerView' returned by call to main actor-isolated function cannot cross actor boundary}}
  let view = CoffeeTrackerView()

  // expected-warning@+4 {{non-sendable type 'CoffeeTrackerView' passed in implicitly asynchronous call to main actor-isolated property 'body' cannot cross actor boundary}}
  // expected-note@+3 {{property access is 'async'}}
  // expected-warning@+2 {{non-sendable type 'some View' in implicitly asynchronous access to main actor-isolated property 'body' cannot cross actor boundary}}
  // expected-error@+1 {{expression is 'async' but is not marked with 'await'}}
  _ = view.body

  // expected-warning@+4 {{non-sendable type 'CoffeeTrackerView' passed in implicitly asynchronous call to main actor-isolated property 'showDrinkList' cannot cross actor boundary}}
  // expected-note@+3 {{property access is 'async'}}
  // expected-warning@+2 {{non-sendable type 'Visibility' in implicitly asynchronous access to main actor-isolated property 'showDrinkList' cannot cross actor boundary}}
  // expected-error@+1 {{expression is 'async' but is not marked with 'await'}}
  _ = view.showDrinkList

  // expected-warning@+2 {{non-sendable type 'CoffeeTrackerView' passed in implicitly asynchronous call to main actor-isolated property 'storage' cannot cross actor boundary}}
  // expected-warning@+1 {{non-sendable type 'Visibility' in implicitly asynchronous access to main actor-isolated property 'storage' cannot cross actor boundary}}
  _ = await view.storage
}
