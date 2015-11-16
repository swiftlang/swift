// RUN: %target-swift-frontend -I %S/../Inputs -enable-source-import -emit-ir -enable-resilience %s | FileCheck %s
// RUN: %target-swift-frontend -I %S/../Inputs -enable-source-import -emit-ir -enable-resilience -O %s

// CHECK-LABEL: %C15enum_resilience5Class = type <{ %swift.refcounted }>
// CHECK-LABEL: %V15enum_resilience9Reference = type <{ %C15enum_resilience5Class* }>

// Public fixed layout struct contains a public resilient struct,
// cannot use spare bits

// CHECK-LABEL: %O15enum_resilience6Either = type <{ [{{4|8}} x i8], [1 x i8] }>

// Public resilient struct contains a public resilient struct,
// can use spare bits (FIXME)

// CHECK-LABEL: %O15enum_resilience15ResilientEither = type <{ [{{4|8}} x i8], [1 x i8] }>

// Internal fixed layout struct contains a public resilient struct,
// can use spare bits (FIXME)

// CHECK-LABEL: %O15enum_resilience14InternalEither = type <{ [{{4|8}} x i8], [1 x i8] }>

// Public fixed layout struct contains a fixed layout struct,
// can use spare bits

// CHECK-LABEL: %O15enum_resilience10EitherFast = type <{ [{{4|8}} x i8] }>

public class Class {}

public struct Reference {
  public var n: Class
}

@_fixed_layout public enum Either {
  case Left(Reference)
  case Right(Reference)
}

public enum ResilientEither {
  case Left(Reference)
  case Right(Reference)
}

enum InternalEither {
  case Left(Reference)
  case Right(Reference)
}

@_fixed_layout public struct ReferenceFast {
  public var n: Class
}

@_fixed_layout public enum EitherFast {
  case Left(ReferenceFast)
  case Right(ReferenceFast)
}

