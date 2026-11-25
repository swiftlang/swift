// rdar://135085246
// RUN: %target-typecheck-verify-swift

protocol View {
  associatedtype Body: View
  @MainActor var body: Body { get }
}

struct Text: View {
  init(_ text: String) {}
  var body: Text { self }
}

@resultBuilder struct ViewBuilder {
  static func buildBlock<Content: View>(_ content: Content) -> Content { content }
}

@resultBuilder struct PreviewMacroBodyBuilder<Content> {
  static func buildBlock(_ content: Content) -> Content { content }
  @available(*, unavailable, message: "This builder requires exactly one content expression")
  static func buildBlock(_ content: Content...) -> Content { fatalError() }
}

class NSView {}
class NSViewController {}
struct ViewTraits {}
struct PreviewTrait<T> {}

@freestanding(declaration)
macro Preview(_ name: String? = nil, @ViewBuilder body: @escaping @MainActor () -> any View) = #externalMacro(module: "PreviewsMacros", type: "SwiftUIView")
// expected-warning@-1 {{external macro implementation type 'PreviewsMacros.SwiftUIView' could not be found for macro 'Preview(_:body:)'; plugin for module 'PreviewsMacros' not found}}
@freestanding(declaration)
macro Preview(_ name: String? = nil, traits: PreviewTrait<ViewTraits>, _ additionalTraits: PreviewTrait<ViewTraits>..., @ViewBuilder body: @escaping @MainActor () -> any View) = #externalMacro(module: "PreviewsMacros", type: "SwiftUIView")
// expected-warning@-1 {{external macro implementation type 'PreviewsMacros.SwiftUIView' could not be found for macro 'Preview(_:traits:_:body:)'; plugin for module 'PreviewsMacros' not found}}
@freestanding(declaration)
macro Preview(_ name: String? = nil, traits: PreviewTrait<ViewTraits>..., @PreviewMacroBodyBuilder<NSView> body: @escaping @MainActor () -> NSView) = #externalMacro(module: "PreviewsMacros", type: "KitViewMacro")
// expected-warning@-1 {{external macro implementation type 'PreviewsMacros.KitViewMacro' could not be found for macro 'Preview(_:traits:body:)'; plugin for module 'PreviewsMacros' not found}}
@freestanding(declaration)
macro Preview(_ name: String? = nil, traits: PreviewTrait<ViewTraits>..., @PreviewMacroBodyBuilder<NSViewController> body: @escaping @MainActor () -> NSViewController) = #externalMacro(module: "PreviewsMacros", type: "KitViewMacro")
// expected-warning@-1 {{external macro implementation type 'PreviewsMacros.KitViewMacro' could not be found for macro 'Preview(_:traits:body:)'; plugin for module 'PreviewsMacros' not found}}

struct ContentView {
  var body: some View {
    Text("Hello")
  }
}

#Preview { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  ContentView()
}
