// RUN: %target-swift-frontend -typecheck %s -target %target-swift-5.1-abi-triple -debug-generic-signatures 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/58301

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

// CHECK-LABEL: .RawDisplayableValue@
// CHECK-NEXT: Requirement signature: <Self where Self : DisplayableValue, Self == Self.[RawDisplayableValue]RawDisplay.[SingleValueDisplay]DisplayedValue, Self.[RawDisplayableValue]RawDisplay : SingleValueDisplay>
public protocol RawDisplayableValue: DisplayableValue {
  associatedtype RawDisplay: SingleValueDisplay
    where RawDisplay.DisplayedValue == Self
}

// CHECK-LABEL: .RawTextDisplayableValue@
// CHECK-NEXT: Requirement signature: <Self where Self : CustomStringConvertible, Self : RawDisplayableValue, Self.[RawDisplayableValue]RawDisplay == RawTextDisplay<Self>>
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
