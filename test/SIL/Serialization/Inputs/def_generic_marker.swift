public protocol mmGeneratorType {
  associatedtype Element
}

public protocol mmSequenceType {
  associatedtype Generator : mmGeneratorType
}

public protocol mmCollectionType : mmSequenceType {
  mutating func extend<
    S : mmSequenceType
    where S.Generator.Element == Self.Generator.Element
  > (_ seq: S)
}

@inlinable
public func test<
  EC1 : mmCollectionType,
  EC2 : mmCollectionType
  where EC1.Generator.Element == EC2.Generator.Element
> (_ lhs: EC1, _ rhs: EC2) -> EC1 {
  var lhs = lhs
  lhs.extend(rhs)
  return lhs
}
