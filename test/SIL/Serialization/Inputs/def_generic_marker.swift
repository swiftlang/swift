public protocol mmGeneratorType {
  typealias Element
}

public protocol mmSequenceType {
  typealias Generator : mmGeneratorType
}

public protocol mmCollectionType : mmSequenceType {
  mutating func extend<
    S : mmSequenceType
    where S.Generator.Element == Self.Generator.Element
  > (seq: S)
}

public func test<
  EC1 : mmCollectionType,
  EC2 : mmCollectionType
  where EC1.Generator.Element == EC2.Generator.Element
> (var lhs: EC1, _ rhs: EC2) -> EC1 {
  lhs.extend(rhs)
  return lhs
}
