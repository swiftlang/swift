// rdar://158045162
// RUN: %target-typecheck-verify-swift

protocol FormatStyle {
  associatedtype FormatInput
  associatedtype FormatOutput
}

protocol ParseableFormatStyle: FormatStyle {}

struct IntegerFormatStyle<Value: BinaryInteger>: ParseableFormatStyle {
  typealias FormatInput = Value
  typealias FormatOutput = String
}

struct FloatingPointFormatStyle<Value: BinaryFloatingPoint>: ParseableFormatStyle {
  typealias FormatInput = Value
  typealias FormatOutput = String
}

extension FormatStyle where Self == IntegerFormatStyle<Int> {
  static var number: Self { fatalError() }
}

extension FormatStyle where Self == FloatingPointFormatStyle<Double> {
  static var number: Self { fatalError() }
}

struct Binding<Value> {}

struct Stepper {
  init<F>(
    value: Binding<F.FormatInput>, step: F.FormatInput.Stride = 1, format: F,
    label: () -> Void
  ) where F: ParseableFormatStyle, F.FormatInput: BinaryFloatingPoint,
          F.FormatOutput == String {}
}

func test(value: Binding<Int>, step: Int) {
  _ = Stepper(value: value, step: step, format: .number) {} // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
