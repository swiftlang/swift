// rdar://106789527
// RUN: %target-typecheck-verify-swift

protocol View {
  associatedtype Body: View
  var body: Body { get }
}

extension Never: View {
  var body: Never { fatalError() }
}

@propertyWrapper
struct Binding<Value> {
  var wrappedValue: Value {
    get { fatalError() }
    nonmutating set {}
  }
  init(get: @escaping () -> Value, set: @escaping (Value) -> Void) {}
}

struct Color {
  static var black: Color { Color() }
}

class CGColor {}

class NSColor {
  class var black: NSColor { NSColor() }
}

struct ColorPicker: View {
  init(_ title: String, selection: Binding<Color>) {}
  init(_ title: String, selection: Binding<CGColor>) {}
  var body: Never { fatalError() }
}

protocol PKTool {}

struct PKInkingTool: PKTool {
  enum InkType { case pen }
  var inkType: InkType
  var color: NSColor
  var width: Double
  init(_ type: InkType, color: NSColor, width: Double) {
    self.inkType = type
    self.color = color
    self.width = width
  }
}

struct DrawingToolbar: View {
  @Binding var selectedTool: PKTool

  var body: some View {
    ColorPicker("Select Color", selection: Binding(get: { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
      (selectedTool as? PKInkingTool)?.color ?? .black
    }, set: { newValue in
      if let tool = selectedTool as? PKInkingTool {
        selectedTool = PKInkingTool(tool.inkType, color: newValue, width: tool.width)
      }
    }))
  }
}
