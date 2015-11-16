import resilient_struct

// Fixed-layout enum with resilient members
@_fixed_layout public enum Shape {
  case Rect(Size)
  case RoundedRect(Size)
}

// Fixed-layout enum with indirect resilient members
@_fixed_layout public enum FunnyShape {
  indirect case Parallelogram(Size)
  indirect case Trapezoid(Size)
}
