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
  open func maroochy(x: Int?, y: Int?) {}
  public struct CityKind {
    public static let Town = 1
  }
}

public protocol ExtraCities {
  func coolum(x: [String : [Int : [(((String))?)]]])
  func blibli(x: (String?, String) -> String!)
  func currimundi(x: (Int, (Int, Int))!)
}

public protocol MoreCities {
  func setZooLocation(x: Int, y: Int, z: Int)
  func addZooAt(_ x: Int, y: Int, z: Int)
}

public func setCityProperty1(_ c : Cities, _ p : Int) {}
public func globalCityFunc() {}
public func setCityProperty2(_ c : Cities, _ p : Int, _ q: Int) {}
public func globalCityFunc2(_ c : Cities) {}
public func globalCityFunc3(_ c : Cities, _ p : Int) -> Int { return 0 }
public func globalCityFunc4(_ c : Cities, _ p : Int, _ q: Int) -> Int { return 0 }
public func globalCityFunc5() -> Int { return 0 }
public func globalCityPointerTaker(_ c : UnsafePointer<Cities>, _ p : Int, _ q: Int) -> Int { return 0 }

public class Container {
  public var Value: String = ""
  public var attrDict: [String: Any] = [:]
  public var attrArr: [String] = []
  public var optionalAttrDict: [String: Any]? = nil
  public func addingAttributes(_ input: [String: Any]) {}
  public func adding(attributes: [String: Any]) {}
  public func adding(optionalAttributes: [String: Any]?) {}
  public init(optionalAttributes: [String: Any]?) {}
  public func adding(attrArray: [String]) {}
  public init(optionalAttrArray: [String]?) {}
  public func add(single: String) {}
  public func add(singleOptional: String?) {}
  public func getAttrArray() -> [String] { return [] }
  public func getOptionalAttrArray() -> [String]? { return [] }
  public func getAttrDictionary() -> [String: Any] { return [:] }
  public func getOptionalAttrDictionary() -> [String: Any]? { return nil }
  public func getSingleAttr() -> String { return "" }
  public func getOptionalSingleAttr() -> String? { return nil }
}
