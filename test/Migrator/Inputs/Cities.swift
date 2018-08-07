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
    public static let Village = 1
  }
}

public protocol ExtraCities {
  func coolum(x: [String : [Int : [(((String))?)]]])
  func blibli(x: (String?, String) -> String?)
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

open class Container {
  public init(optionalAttributes: [String: Any]?) {}
  public init(optionalAttrArray: [String]?) {}

  open func adding(attrArray: [String]) {}
  open var Value: String = ""
  open var attrDict: [String: Any] = [:]
  open var attrArr: [String] = []
  open var optionalAttrDict: [String: Any]? = nil
  open func addingAttributes(_ input: [String: Any]) {}
  open func adding(attributes: [String: Any]) {}
  open func adding(optionalAttributes: [String: Any]?) {}
  open func add(single: String) {}
  open func add(singleOptional: String?) {}
  open func getAttrArray() -> [String] { return [] }
  open func getOptionalAttrArray() -> [String]? { return [] }
  open func getAttrDictionary() -> [String: Any] { return [:] }
  open func getOptionalAttrDictionary() -> [String: Any]? { return nil }
  open func getSingleAttr() -> String { return "" }
  open func getOptionalSingleAttr() -> String? { return nil }
}

open class ToplevelType {
  public init() {}
  public init(recordName: String) {}
  open func member(_ x: @escaping ([Any]?) -> Void) {}
}

public var GlobalAttribute: String = ""

public enum FontWeighting: Int {
  case Light = 0
  case Regular
  case Bold
}

public enum FontWeight: Int {
  case Light = 0
  case Regular
  case Bold
}

public struct AwesomeCityAttribute: RawRepresentable {
  public init(rawValue: String) { self.rawValue = rawValue }
  public init(_ rawValue: String) { self.rawValue = rawValue }
  public var rawValue: String
  public typealias RawValue = String
}

public class Wrapper {
  public struct Attribute: RawRepresentable {
    public static let KnownAttr = Wrapper.Attribute(rawValue: "")
    public init(rawValue: String) { self.rawValue = rawValue }
    public init(_ rawValue: String) { self.rawValue = rawValue }
    public var rawValue: String
    public typealias RawValue = String
  }
}

public typealias AliasAttribute = String
