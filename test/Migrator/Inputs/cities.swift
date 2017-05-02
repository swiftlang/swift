open class Cities {
  var x: Int
  public init(x: Int) { self.x = x }
  public init!(y: Int) { self.x = y }
  open func mooloolaba(x: Cities, y: Cities?) {}
  open func toowoomba(x: [Cities], y: [Cities]?) {}
  open func mareeba(x: [String : Cities?], y: [String : Cities]?) {}
  open func yandina(x: [[String : Cities]]!) {}
  open func buderim() -> Cities? { return Cities(x: 1) }
  open func noosa() -> [[String : Cities]?] { return [] }
}

public protocol ExtraCities {
  func coolum(x: [String : [Int : [(((String))?)]]])
  func blibli(x: (String?, String) -> String!)
  func currimundi(x: (Int, (Int, Int))!)
}

public protocol MoreCities {
  func setZooLocation(x: Int, y: Int, z: Int)
}