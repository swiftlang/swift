// rdar://126279538
// RUN: %target-typecheck-verify-swift

protocol View {
  associatedtype Body: View
  var body: Body { get }
}

extension Never: View {
  var body: Never { fatalError() }
}

struct Text: View {
  init(_: String) {}
  var body: Never { fatalError() }
}

struct Button<Label: View>: View {
  init(action: @escaping () -> Void, label: () -> Label) {}
  var body: Never { fatalError() }
}

struct Row<Content: View>: View {
  var label: () -> Content
  var control: () -> Content

  var body: Never { fatalError() }
}

struct ContainerDemo: View {
  var body: some View {
    Row(label: { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
      Text("label")
    }, control: {
      Button(action: {}, label: { Text("control") })
    })
  }
}
