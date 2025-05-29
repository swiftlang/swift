import resilient_struct

// Fixed-layout enum with resilient members
package enum SimpleShape {
  case KleinBottle
  case Triangle(Size)
}

public enum PublicSimpleShape {
  case pbKleinBottle
  case pbTriangle(PublicSize)
}

// Fixed-layout enum with resilient members
package enum Shape {
  case Point
  case Rect(Size)
  case RoundedRect(Size, Size)
}

// Fixed-layout enum with indirect resilient members
package enum FunnyShape {
  indirect case Parallelogram(Size)
  indirect case Trapezoid(Size)
}

public enum PublicFunnyShape {
  indirect case Parallelogram(PublicSize)
  indirect case Trapezoid(PublicSize)
}

package enum FullyFixedLayout {
  case noPayload
  case hasPayload(Int)
}

// The enum payload has fixed layout inside this module, but
// resilient layout outside. Make sure we emit the payload
// size in the metadata.

package struct Color {
  package let r: Int, g: Int, b: Int

  package init(r: Int, g: Int, b: Int) {
    self.r = r
    self.g = g
    self.b = b
  }
}

package enum CustomColor {
  case Black
  case White
  case Custom(Color)
  case Bespoke(Color, Color)
}

// Resilient enum
package enum Medium {
  // Indirect case
  indirect case Pamphlet(Medium)  // -1

  // Case with resilient payload
  case Postcard(Size)             // -2

  // Empty cases
  case Paper                      // 0
  case Canvas                     // 1
}

public enum PublicMedium {
  // Indirect case
  indirect case Pamphlet(PublicMedium)  // -1

  // Case with resilient payload
  case Postcard(PublicSize)             // -2

  // Empty cases
  case Paper                      // 0
  case Canvas                     // 1
}

// Indirect resilient enum
package indirect enum IndirectApproach {
  case Angle(Double)              // -1
}

// Resilient enum with resilient empty payload case
package struct EmptyStruct {
  package init() {}
}

package enum ResilientEnumWithEmptyCase {
  case A                          // 0
  case B                          // 1
  case Empty(EmptyStruct)         // -1
}

package func getResilientEnumWithEmptyCase() -> [ResilientEnumWithEmptyCase] {
  return [.A, .B, .Empty(EmptyStruct())]
}

// Specific enum implementations for executable tests
package enum ResilientEmptyEnum {
  case X
}

package enum ResilientSingletonEnum {
  case X(AnyObject)
}

package enum ResilientSingletonGenericEnum<T> {
  case X(T)
}

package enum ResilientNoPayloadEnum {
  case A
  case B
  case C
}

package enum ResilientSinglePayloadEnum {
  case X(AnyObject)               // -1
  case A                          // 0
  case B                          // 1
  case C                          // 2
}

package enum ResilientSinglePayloadGenericEnum<T> {
  case X(T)                       // -1
  case A                          // 0
  case B                          // 1
  case C                          // 2
}

package class ArtClass {
  package init() {}
}

package enum ResilientMultiPayloadEnum {
  case A
  case B
  case C
  case X(Int)
  case Y(Int)
}

package func makeResilientMultiPayloadEnum(_ n: Int, i: Int)
    -> ResilientMultiPayloadEnum {
  switch i {
  case 0:
    return .A
  case 1:
    return .B
  case 2:
    return .C
  case 3:
    return .X(n)
  case 4:
    return .Y(n)
  default:
    while true {}
  }
}

package enum ResilientMultiPayloadEnumSpareBits {
  case A                          // 0
  case B                          // 1
  case C                          // 2
  case X(ArtClass)                // -1
  case Y(ArtClass)                // -2
}

package func makeResilientMultiPayloadEnumSpareBits(_ o: ArtClass, i: Int)
    -> ResilientMultiPayloadEnumSpareBits {
  switch i {
  case 0:
    return .A
  case 1:
    return .B
  case 2:
    return .C
  case 3:
    return .X(o)
  case 4:
    return .Y(o)
  default:
    while true {}
  }
}

package typealias SevenSpareBits = (Bool, Int8, Int8, Int8, Int8, Int8, Int8, Int8)

package enum ResilientMultiPayloadEnumSpareBitsAndExtraBits {
  // On 64-bit little endian, 7 spare bits at the LSB end
  case P1(SevenSpareBits)
  // On 64-bit, 8 spare bits at the MSB end and 3 at the LSB end
  case P2(ArtClass)
  case P3(ArtClass)
  case P4(ArtClass)
  case P5(ArtClass)
  case P6(ArtClass)
  case P7(ArtClass)
  case P8(ArtClass)
}

package enum ResilientMultiPayloadGenericEnum<T> {
  case A                          // 0
  case B                          // 1
  case C                          // 2
  case X(T)                       // -1
  case Y(T)                       // -2
}

package enum ResilientMultiPayloadGenericEnumFixedSize<T> {
  case A                          // 0
  case B                          // 1
  case C                          // 2
  case X(Int)                     // -1
  case Y(Int)                     // -2
}

package enum ResilientIndirectEnum {
  // 0
  case Base

  // -1
  indirect case A(ResilientIndirectEnum)

  // -2
  indirect case B(ResilientIndirectEnum, ResilientIndirectEnum)
}

package enum ResilientEnumWithUnavailableCase {
  case available

  @available(*, unavailable)
  case unavailable
}
