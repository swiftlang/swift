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

// Resilient enum
public enum Medium {
  // Empty cases
  case Paper
  case Canvas

  // Indirect case
  indirect case Pamphlet(Medium)

  // Case with resilient payload
  case Postcard(Size)
}

// Indirect resilient enum
public indirect enum IndirectApproach {
  case Angle(Double)
}
