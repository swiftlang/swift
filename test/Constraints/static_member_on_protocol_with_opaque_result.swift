// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -typecheck -verify %s

// rdar://75978086 - static member lookup doesn't work with opaque types

protocol Intent {}

extension Intent where Self == Intents.List {
  static func orderedList() -> Self {
    return Intents.List.orderedList(nestedIn: nil)
  }
}

enum Intents {
  enum List: Intent {
  case orderedList(nestedIn: Any?)
  }
}

let rdar75978086: some Intent = .orderedList() // Ok
