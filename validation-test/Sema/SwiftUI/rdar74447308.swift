// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

struct Item {
  let id = UUID()
  let text = ""
}

class ItemList: ObservableObject {
  @Published var items = [Item]()
}

struct ContentView: View {
  @StateObject var list = ItemList()

  var body: some View {
    List {
      ForEach(list.items) { item in
      // expected-error@-1 {{referencing initializer 'init(_:content:)' on 'ForEach' requires that 'Item' conform to 'Identifiable'}}
        Text(item.text)
      }
    }
  }
}
