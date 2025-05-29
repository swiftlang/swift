// RUN: %target-swift-emit-silgen %s -target %target-swift-5.9-abi-triple

// Make sure we can lower all of these types without crashing.

public struct G1<each T> {
  public let t: (repeat each T)
}

public struct S1 {
  public let g: G1< >
  public let gg: G1<Int>
  public let ggg: G1<Int, Float>
}

public struct G2<each T> {
  public let t: (repeat (each T).Type)
}

public struct S2 {
  public let g: G2< >
  public let gg: G2<Int>
  public let ggg: G2<Int, Float>
}

public struct G3<each T> {
  public let t: (repeat (each T) -> ())
}

public struct S3 {
  public let g: G3< >
  public let gg: G3<Int>
  public let ggg: G3<Int, Float>
}

public struct G4<each T> {
  public let t: (repeat ((each T).Type) -> ())
}

public struct S4 {
  public let g: G4< >
  public let gg: G4<Int>
  public let ggg: G4<Int, Float>
}
