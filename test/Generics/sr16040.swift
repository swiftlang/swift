// RUN: %target-swift-frontend -typecheck -verify %s -disable-availability-checking

public protocol View {
  associatedtype Body : View
  var body: Body { get }
}

public struct Text : View {
  public init(_: String) {}
  public var body: Self { return self }
}

public protocol DisplayableValue {}

public protocol SingleValueDisplay: View {
  associatedtype DisplayedValue
  init (_ singleValue: DisplayedValue)
  var displayedValue: DisplayedValue { get }
}

public protocol RawDisplayableValue: DisplayableValue {
  associatedtype RawDisplay: SingleValueDisplay
    where RawDisplay.DisplayedValue == Self
}

public protocol RawTextDisplayableValue: RawDisplayableValue
  where Self: CustomStringConvertible,
        RawDisplay == RawTextDisplay<Self> { }

public struct RawTextDisplay <Value: CustomStringConvertible>: SingleValueDisplay {
  public var displayedValue: Value
  
  public init (_ singleValue: Value) {
    self.displayedValue = singleValue
  }
  
  public var body: some View {
    Text(displayedValue.description)
  }
}
