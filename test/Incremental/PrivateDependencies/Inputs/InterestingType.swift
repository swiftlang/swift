protocol IntMaker {}
extension IntMaker {
  func make() -> Int { return 0 }
}

protocol DoubleMaker {}
extension DoubleMaker {
  func make() -> Double { return 0 }
}


#if OLD
typealias InterestingType = Int
typealias InterestingProto = IntMaker

#elseif NEW
typealias InterestingType = Double
typealias InterestingProto = DoubleMaker

#else
typealias InterestingType = ErrorMustSetOLDOrNew

#endif
