infix operator .& : LogicalConjunctionPrecedence
infix operator .^ : LogicalDisjunctionPrecedence
infix operator .| : LogicalDisjunctionPrecedence
infix operator .&= : AssignmentPrecedence
infix operator .^= : AssignmentPrecedence
infix operator .|= : AssignmentPrecedence
prefix operator .!

//  Implementations of integer operations. These should eventually all
//  be replaced with @_semantics to lower directly to vector IR nodes.
public extension SIMD where Scalar : FixedWidthInteger {
  @_transparent
  var leadingZeroBitCount: Self {
    var result = Self()
    for i in indices { result[i] = Scalar(self[i].leadingZeroBitCount) }
    return result
  }
  
  @_transparent
  var trailingZeroBitCount: Self {
    var result = Self()
    for i in indices { result[i] = Scalar(self[i].trailingZeroBitCount) }
    return result
  }
  
  @_transparent
  var nonzeroBitCount: Self {
    var result = Self()
    for i in indices { result[i] = Scalar(self[i].nonzeroBitCount) }
    return result
  }
  
  @_transparent
  static prefix func ~(rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = ~rhs[i] }
    return result
  }
  
  @_transparent
  static func &(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] & rhs[i] }
    return result
  }
  
  @_transparent
  static func ^(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] ^ rhs[i] }
    return result
  }
  
  @_transparent
  static func |(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] | rhs[i] }
    return result
  }
  
  @_transparent
  static func &<<(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] &<< rhs[i] }
    return result
  }
  
  @_transparent
  static func &>>(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] &>> rhs[i] }
    return result
  }
  
  @_transparent
  static func &+(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] &+ rhs[i] }
    return result
  }
  
  @_transparent
  static func &-(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] &- rhs[i] }
    return result
  }
  
  @_transparent
  static func &*(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] &* rhs[i] }
    return result
  }
  
  @_transparent
  static func /(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] / rhs[i] }
    return result
  }
  
  @_transparent
  static func %(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] % rhs[i] }
    return result
  }
}

//  Implementations of floating-point operations. These should eventually all
//  be replaced with @_semantics to lower directly to vector IR nodes.
public extension SIMD where Scalar : FloatingPoint {
  @_transparent
  static func +(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] + rhs[i] }
    return result
  }
  
  @_transparent
  static func -(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] - rhs[i] }
    return result
  }
  
  @_transparent
  static func *(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] * rhs[i] }
    return result
  }
  
  @_transparent
  static func /(lhs: Self, rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = lhs[i] / rhs[i] }
    return result
  }
  
  @_transparent
  func addingProduct(_ lhs: Self, _ rhs: Self) -> Self {
    var result = Self()
    for i in result.indices { result[i] = self[i].addingProduct(lhs[i], rhs[i]) }
    return result
  }
  
  @_transparent
  func squareRoot( ) -> Self {
    var result = Self()
    for i in result.indices { result[i] = self[i].squareRoot() }
    return result
  }
  
  @_transparent
  func rounded(_ rule: FloatingPointRoundingRule) -> Self {
    var result = Self()
    for i in result.indices { result[i] = self[i].rounded(rule) }
    return result
  }
}

public extension SIMDMask {
  @_transparent
  static prefix func .!(rhs: SIMDMask) -> SIMDMask {
    return SIMDMask(~rhs._storage)
  }
  
  @_transparent
  static func .&(lhs: SIMDMask, rhs: SIMDMask) -> SIMDMask {
    return SIMDMask(lhs._storage & rhs._storage)
  }
  
  @_transparent
  static func .^(lhs: SIMDMask, rhs: SIMDMask) -> SIMDMask {
    return SIMDMask(lhs._storage ^ rhs._storage)
  }
  
  @_transparent
  static func .|(lhs: SIMDMask, rhs: SIMDMask) -> SIMDMask {
    return SIMDMask(lhs._storage | rhs._storage)
  }
}

//  These operations should never need @_semantics; they should be trivial
//  wrappers around the core operations defined above.
public extension SIMD where Scalar : FixedWidthInteger {
  @_transparent static func &(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) & rhs }
  @_transparent static func ^(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) ^ rhs }
  @_transparent static func |(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) | rhs }
  @_transparent static func &<<(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) &<< rhs }
  @_transparent static func &>>(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) &>> rhs }
  @_transparent static func &+(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) &+ rhs }
  @_transparent static func &-(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) &- rhs }
  @_transparent static func &*(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) &* rhs }
  @_transparent static func /(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) / rhs }
  @_transparent static func %(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) % rhs }
  
  @_transparent static func &(lhs: Self, rhs: Scalar) -> Self { return lhs & Self(repeating: rhs) }
  @_transparent static func ^(lhs: Self, rhs: Scalar) -> Self { return lhs ^ Self(repeating: rhs) }
  @_transparent static func |(lhs: Self, rhs: Scalar) -> Self { return lhs | Self(repeating: rhs) }
  @_transparent static func &<<(lhs: Self, rhs: Scalar) -> Self { return lhs &<< Self(repeating: rhs) }
  @_transparent static func &>>(lhs: Self, rhs: Scalar) -> Self { return lhs &>> Self(repeating: rhs) }
  @_transparent static func &+(lhs: Self, rhs: Scalar) -> Self { return lhs &+ Self(repeating: rhs) }
  @_transparent static func &-(lhs: Self, rhs: Scalar) -> Self { return lhs &- Self(repeating: rhs) }
  @_transparent static func &*(lhs: Self, rhs: Scalar) -> Self { return lhs &* Self(repeating: rhs) }
  @_transparent static func /(lhs: Self, rhs: Scalar) -> Self { return lhs / Self(repeating: rhs) }
  @_transparent static func %(lhs: Self, rhs: Scalar) -> Self { return lhs % Self(repeating: rhs) }
  
  @_transparent static func &=(lhs: inout Self, rhs: Self) { lhs = lhs & rhs }
  @_transparent static func ^=(lhs: inout Self, rhs: Self) { lhs = lhs ^ rhs }
  @_transparent static func |=(lhs: inout Self, rhs: Self) { lhs = lhs | rhs }
  @_transparent static func &<<=(lhs: inout Self, rhs: Self) { lhs = lhs &<< rhs }
  @_transparent static func &>>=(lhs: inout Self, rhs: Self) { lhs = lhs &>> rhs }
  @_transparent static func &+=(lhs: inout Self, rhs: Self) { lhs = lhs &+ rhs }
  @_transparent static func &-=(lhs: inout Self, rhs: Self) { lhs = lhs &- rhs }
  @_transparent static func &*=(lhs: inout Self, rhs: Self) { lhs = lhs &* rhs }
  @_transparent static func /=(lhs: inout Self, rhs: Self) { lhs = lhs / rhs }
  @_transparent static func %=(lhs: inout Self, rhs: Self) { lhs = lhs % rhs }
  
  @_transparent static func &=(lhs: inout Self, rhs: Scalar) { lhs = lhs & rhs }
  @_transparent static func ^=(lhs: inout Self, rhs: Scalar) { lhs = lhs ^ rhs }
  @_transparent static func |=(lhs: inout Self, rhs: Scalar) { lhs = lhs | rhs }
  @_transparent static func &<<=(lhs: inout Self, rhs: Scalar) { lhs = lhs &<< rhs }
  @_transparent static func &>>=(lhs: inout Self, rhs: Scalar) { lhs = lhs &>> rhs }
  @_transparent static func &+=(lhs: inout Self, rhs: Scalar) { lhs = lhs &+ rhs }
  @_transparent static func &-=(lhs: inout Self, rhs: Scalar) { lhs = lhs &- rhs }
  @_transparent static func &*=(lhs: inout Self, rhs: Scalar) { lhs = lhs &* rhs }
  @_transparent static func /=(lhs: inout Self, rhs: Scalar) { lhs = lhs / rhs }
  @_transparent static func %=(lhs: inout Self, rhs: Scalar) { lhs = lhs % rhs }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&+' instead")
  static func +(lhs: Self, rhs: Self) -> Self { fatalError() }
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&-' instead")
  static func -(lhs: Self, rhs: Self) -> Self { fatalError() }
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&*' instead")
  static func *(lhs: Self, rhs: Self) -> Self { fatalError() }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&+' instead")
  static func +(lhs: Self, rhs: Scalar) -> Self { fatalError() }
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&-' instead")
  static func -(lhs: Self, rhs: Scalar) -> Self { fatalError() }
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&*' instead")
  static func *(lhs: Self, rhs: Scalar) -> Self { fatalError() }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&+' instead")
  static func +(lhs: Scalar, rhs: Self) -> Self { fatalError() }
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&-' instead")
  static func -(lhs: Scalar, rhs: Self) -> Self { fatalError() }
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&*' instead")
  static func *(lhs: Scalar, rhs: Self) -> Self { fatalError() }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&+=' instead")
  static func +=(lhs: inout Self, rhs: Self) { fatalError() }
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&-=' instead")
  static func -=(lhs: inout Self, rhs: Self) { fatalError() }
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&*=' instead")
  static func *=(lhs: inout Self, rhs: Self) { fatalError() }
  
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&+=' instead")
  static func +=(lhs: inout Self, rhs: Scalar) { fatalError() }
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&-=' instead")
  static func -=(lhs: inout Self, rhs: Scalar) { fatalError() }
  @available(*, unavailable, message: "integer vector types do not support checked arithmetic; use the wrapping operator '&*=' instead")
  static func *=(lhs: inout Self, rhs: Scalar) { fatalError() }
}

public extension SIMD where Scalar : FloatingPoint {
  @_transparent static prefix func -(rhs: Self) -> Self { return 0 - rhs }
  
  @_transparent static func +(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) + rhs }
  @_transparent static func -(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) - rhs }
  @_transparent static func *(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) * rhs }
  @_transparent static func /(lhs: Scalar, rhs: Self) -> Self { return Self(repeating: lhs) / rhs }
  
  @_transparent static func +(lhs: Self, rhs: Scalar) -> Self { return lhs + Self(repeating: rhs) }
  @_transparent static func -(lhs: Self, rhs: Scalar) -> Self { return lhs - Self(repeating: rhs) }
  @_transparent static func *(lhs: Self, rhs: Scalar) -> Self { return lhs * Self(repeating: rhs) }
  @_transparent static func /(lhs: Self, rhs: Scalar) -> Self { return lhs / Self(repeating: rhs) }
  
  @_transparent static func +=(lhs: inout Self, rhs: Self) { lhs = lhs + rhs }
  @_transparent static func -=(lhs: inout Self, rhs: Self) { lhs = lhs - rhs }
  @_transparent static func *=(lhs: inout Self, rhs: Self) { lhs = lhs * rhs }
  @_transparent static func /=(lhs: inout Self, rhs: Self) { lhs = lhs / rhs }
  
  @_transparent static func +=(lhs: inout Self, rhs: Scalar) { lhs = lhs + rhs }
  @_transparent static func -=(lhs: inout Self, rhs: Scalar) { lhs = lhs - rhs }
  @_transparent static func *=(lhs: inout Self, rhs: Scalar) { lhs = lhs * rhs }
  @_transparent static func /=(lhs: inout Self, rhs: Scalar) { lhs = lhs / rhs }
  
  @_transparent func addingProduct(_ lhs: Scalar, _ rhs: Self) -> Self {
    return self.addingProduct(Self(repeating: lhs), rhs)
  }
  @_transparent func addingProduct(_ lhs: Self, _ rhs: Scalar) -> Self {
    return self.addingProduct(lhs, Self(repeating: rhs))
  }
  @_transparent mutating func addProduct(_ lhs: Self, _ rhs: Self) {
    self = self.addingProduct(lhs, rhs)
  }
  @_transparent mutating func addProduct(_ lhs: Scalar, _ rhs: Self) {
    self = self.addingProduct(lhs, rhs)
  }
  @_transparent mutating func addProduct(_ lhs: Self, _ rhs: Scalar) {
    self = self.addingProduct(lhs, rhs)
  }
  
  @_transparent mutating func formSquareRoot( ) {
    self = self.squareRoot()
  }
  
  @_transparent mutating func round(_ rule: FloatingPointRoundingRule) {
    self = self.rounded(rule)
  }
}

public extension SIMDMask {
  @_transparent static func .&(lhs: Bool, rhs: SIMDMask) -> SIMDMask { return SIMDMask(repeating: lhs) .& rhs }
  @_transparent static func .^(lhs: Bool, rhs: SIMDMask) -> SIMDMask { return SIMDMask(repeating: lhs) .^ rhs }
  @_transparent static func .|(lhs: Bool, rhs: SIMDMask) -> SIMDMask { return SIMDMask(repeating: lhs) .| rhs }
  
  @_transparent static func .&(lhs: SIMDMask, rhs: Bool) -> SIMDMask { return lhs .& SIMDMask(repeating: rhs) }
  @_transparent static func .^(lhs: SIMDMask, rhs: Bool) -> SIMDMask { return lhs .^ SIMDMask(repeating: rhs) }
  @_transparent static func .|(lhs: SIMDMask, rhs: Bool) -> SIMDMask { return lhs .| SIMDMask(repeating: rhs) }
  
  @_transparent static func .&=(lhs: inout SIMDMask, rhs: SIMDMask) { lhs = lhs .& rhs }
  @_transparent static func .^=(lhs: inout SIMDMask, rhs: SIMDMask) { lhs = lhs .^ rhs }
  @_transparent static func .|=(lhs: inout SIMDMask, rhs: SIMDMask) { lhs = lhs .| rhs }
  
  @_transparent static func .&=(lhs: inout SIMDMask, rhs: Bool) { lhs = lhs .& rhs }
  @_transparent static func .^=(lhs: inout SIMDMask, rhs: Bool) { lhs = lhs .^ rhs }
  @_transparent static func .|=(lhs: inout SIMDMask, rhs: Bool) { lhs = lhs .| rhs }
}
