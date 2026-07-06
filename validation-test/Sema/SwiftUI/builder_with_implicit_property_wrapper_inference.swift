// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

struct Example: View {
  struct Item: Identifiable {
    var id: Int
    var title: String
    var isOn: Bool = .random()
  }

  @State var items = [
    Item(id: 0, title: "")
  ]

  var body: some View {
    List {
      Section {
        ForEach($items) { $item in
          Toggle(item.title, isOn: $item.isOn.animation())
        }
        .onDelete { items.remove(atOffsets: $0) }
      }
    }
  }
}
