// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

final class Item: Identifiable, ObservableObject {
  var id = 42
  var text: String

  init(index: Int) {
    self.text = "Item \(index)"
  }
}

enum MessageKind: CaseIterable, Hashable, Identifiable {
  var id: Self { self }

  case channel
  case system
}

struct ContentView: View {
  @State var items = (0..<100).map { Item(index: $0) }
  var body: some View {
    List {
      ForEach($items) { $item in
        HStack {
          Text(item.text)
          ForEach(MessageKind.allCases) { kind in
            Text("\(kind.noSuchMember)") // expected-error {{value of type 'MessageKind' has no member 'noSuchMember'}}
          }
        }
      }
    }
  }
}
