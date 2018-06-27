public protocol mmGeneratorType {
  associatedtype Element
}

public protocol mmSequenceType {
  associatedtype Generator : mmGeneratorType
}

public protocol mmCollectionType : mmSequenceType {
  mutating func extend<
    S : mmSequenceType
  > (_ seq: S) where S.Generator.Element == Self.Generator.Element
}

@inlinable
public func test<
  EC1 : mmCollectionType,
  EC2 : mmCollectionType
> (_ lhs: EC1, _ rhs: EC2) -> EC1
  where EC1.Generator.Element == EC2.Generator.Element
{
  var lhs = lhs
  lhs.extend(rhs)
  return lhs
}
