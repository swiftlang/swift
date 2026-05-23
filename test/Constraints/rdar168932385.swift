// RUN: %target-typecheck-verify-swift

// rdar://168932385 - error: type 'Comparable' has no member 'pi'

protocol FormatStyle<FormatInput, FormatOutput> {
  associatedtype FormatInput
  associatedtype FormatOutput

  func format(_ value: Self.FormatInput) -> Self.FormatOutput
}

protocol ParseStrategy {
  associatedtype ParseInput
  associatedtype ParseOutput

  func parse(_ value: Self.ParseInput) throws -> Self.ParseOutput
}

protocol ParseableFormatStyle : FormatStyle {
  associatedtype Strategy : ParseStrategy where Self.FormatInput == Self.Strategy.ParseOutput, Self.FormatOutput == Self.Strategy.ParseInput

  var parseStrategy: Self.Strategy { get }
}

protocol ScalarFormatStyle: ParseableFormatStyle
  where FormatInput: Hashable, FormatOutput == String {
}

extension ParseableFormatStyle where Self: ParseStrategy {
    var parseStrategy: Self { self }
}

struct AngleFormatStyle<T: BinaryFloatingPoint>: ScalarFormatStyle, ParseStrategy {
  func format(_ value: T) -> String { "" }
  func parse(_ str: String) throws -> T { }
}

extension FormatStyle where Self == AngleFormatStyle<Float> {
  static var angle: Self { Self() }
}

protocol CellValue: Hashable {
}

extension Float: CellValue {}

enum Cell<Value: CellValue>: Hashable {
}

struct SliderCellField<F: ScalarFormatStyle> where
   F.FormatInput: CellValue & BinaryFloatingPoint,
   F.FormatInput.Stride: BinaryFloatingPoint
{
  var value: Cell<F.FormatInput>
  var range: ClosedRange<F.FormatInput>
  var format: F
}

func test(cell: Cell<Float>) {
  _ = SliderCellField(value: cell, range: -.pi ... .pi, format: .angle) // Ok
}
