// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5 -disable-availability-checking

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

protocol Model<ReturnType> {
  associatedtype ReturnType
}

struct AnyModel<ReturnType>: Model {
}

protocol ContentProtocol : View {
  associatedtype _Context
}

struct CollectionContext<Data: RandomAccessCollection> {
  let offset: Data.Index
}

struct ContinuousContent<Data: RandomAccessCollection, Content: View> : ContentProtocol
  where Data.Element: Model, Data.Element.ReturnType: Sequence, Data.Index: Hashable {

  typealias _Context = CollectionContext<Data>

  var body: some View { EmptyView() }
}

struct TestView<Data, Content: View> : View {
   typealias Context = Content._Context where Content: ContentProtocol

   init<R, C>(_ data: Data,
              @ViewBuilder shelfContent: @escaping ([Context]) -> C)
       where Data.Element == any Model<R>,
             Content == ContinuousContent<LazyMapCollection<Data, AnyModel<R>>, C> {
   }

   var body: some View { EmptyView() }
}

@ViewBuilder
func test(values: [any Model<[Int]>]) -> some View {
  TestView(values) { context in
    VStack {
      if context.first?.offset == 0 {
      }
    }
  }
}
