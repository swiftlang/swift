// rdar://125735258
// RUN: %target-typecheck-verify-swift

protocol FormatStyle {
  associatedtype FormatInput
}

struct IntegerFormatStyle<Value: BinaryInteger>: FormatStyle {
  typealias FormatInput = Value
}

struct FloatingPointFormatStyle<Value: BinaryFloatingPoint>: FormatStyle {
  typealias FormatInput = Value
}

extension FormatStyle where Self == IntegerFormatStyle<Int> {
  static var number: Self { fatalError() }
}

extension FormatStyle where Self == FloatingPointFormatStyle<Double> {
  static var number: Self { fatalError() }
}

extension FormatStyle where Self == FloatingPointFormatStyle<Float> {
  static var number: Self { fatalError() }
}

struct Binding<Value> {}

struct Stepper {
  init<F: FormatStyle>(
    _ title: String, value: Binding<F.FormatInput>,
    step: F.FormatInput.Stride = 1, format: F
  ) where F.FormatInput: BinaryFloatingPoint {}
}

func test(value: Binding<Int>) {
  _ = Stepper("", value: value, format: .number) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
