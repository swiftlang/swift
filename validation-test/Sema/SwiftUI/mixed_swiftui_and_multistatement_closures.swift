// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

enum Status {
  case complete
  case waiting
}

struct Item : Hashable {
  var question: String
  var answer: Int
}

func transform(_ v: Int) -> String? { return String(v) }
func transform(_ v: Double) -> String? { return String(v) }

struct MyView : View {
  var status: Status

  var items: [Item] = []

  var currItem: Item {
    get { fatalError() }
    nonmutating set { }
  }

  var body: some View {
    ZStack {
      ItemsView {
        EmptyView()
      } results: {
        switch (status) {
        case .complete:
          ForEach(items, id: \.self) { item in
            if let question = item.question,
               let answer = item.answer {
              ItemView {
                currItem.question = question
                currItem.answer = answer
              } content: {
                AnswerView(title: "",
                           color: .red,
                           answer: transform(answer) ?? "",
                           selected: false)
              }
            }
          }

        default:
          EmptyView()
        }
      }
    }
  }
}

struct AnswerView : View {
  var title: String
  var color: Color
  var answer: String
  var selected: Bool

  var body: some View {
    EmptyView()
  }
}

struct ItemsView<Content: View,  Results: View>: View {
  @ViewBuilder var content: Content
  @ViewBuilder var results: Results

  var body: some View {
    EmptyView()
  }
}

struct ItemView<Content: View> : View {
  var fn: () -> Void
  @ViewBuilder var content: Content

  var body: some View {
    EmptyView()
  }
}
