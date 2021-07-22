// RUN: %swift-ide-test --code-completion --source-filename %s --code-completion-token=CC

struct Listing {}

protocol View2 {}

extension View2 {
  @available(macOS 10.15, *)
  func onTapGesturf(perform action: () -> Void) -> some View2 { fatalError() }
}

@resultBuilder struct ViewBuilder2 {
  static func buildBlock() -> Never { fatalError() }
  static func buildBlock<Content>(_ content: Content) -> Content where Content : View2 { fatalError() }
}

struct HStack2<Content> : View2 {
  init(@ViewBuilder2 content: () -> Content) { fatalError() }
}

struct ItemImage2 : View2 {
  init(path: String) {}
}

struct ForEach2<Data, Content>: View2 {
  init(_ data: [Listing], @ViewBuilder2 content: (Data) -> Content) {}
}


struct TodayNookazonSection {

  let listings: [Listing]

  @available(macOS 10.15, *)
  @ViewBuilder2 var body: some View2 {
    ForEach2(listings) { listing in
      HStack2 {
        HStack2 {
          ItemImage2(path#^CC^#: "abc")
        }
        .onTapGesturf {
          listing
        }
      }
    }
  }
}
