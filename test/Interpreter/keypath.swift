// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-feature -Xfrontend KeyPathWithMethodMembers) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_KeyPathWithMethodMembers

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

class MyLabel: Hashable {
  var text = "label"
  static var isVisible = true
  func x(val value: Int) -> Int { return value }
  static func y(val value: Int) -> Int { return value }
  func saveClosure(_ closure: @escaping () -> Void) {
    storedClosure = closure
  }
  func executeStoredClosure() {
    storedClosure?()
  }
  private var storedClosure: (() -> Void)?
  
  required init() {}
  required init(customText: String) {
      text = customText
  }
  
  static func == (lhs: MyLabel, rhs: MyLabel) -> Bool {
      return lhs === rhs
  }
  func hash(into hasher: inout Hasher) {
      hasher.combine(ObjectIdentifier(self))
  }
}

class Controller {
  
  fileprivate let label = MyLabel()
  fileprivate var secondLabel: MyLabel? = MyLabel()
  public var thirdLabel: MyLabel? = MyLabel()
  fileprivate var fourthLabel: MyLabel.Type? { return MyLabel.self }
  public var fifthLabel: MyLabel.Type? { return MyLabel.self }
  
  subscript(string: String) -> String {
    get {
      ""
    }
    set {
      
    }
  }
  
  subscript(int int: Int, str str: String, otherInt: Int) -> Int {
    get {
      0
    }
    set {
      
    }
  }
  
  subscript() -> Int {
    0
  }
  
  subscript<T>(array: [T]) -> T? {
    array.first
  }

  subscript<T, U>(array: [T], array2: [U]) -> T? {
    array.first
  }

  subscript<T>(array array: [T]) -> T? {
    array.first
  }

  subscript<T, U>(array array: [T], array2 array2: [U]) -> T? {
    array.first
  }
}

struct Container<V> {
  var v : V
  init(_ v: V) {
    self.v = v
  }
  func useKeyPath<V2: AnyObject>(_ keyPath: KeyPath<V, V2>) -> String {
    return (v[keyPath: keyPath] as! MyLabel).text
  }
  func invokeKeyPathMethod<V2, R>(
      _ keyPath: KeyPath<V, V2>,
      method: KeyPath<V2, (Int) -> R>,
      arg: Int
    ) -> R {
    let instance = v[keyPath: keyPath]
    return instance[keyPath: method](arg)
  }
}

extension Container where V: Controller {
  func test() -> String {
    return useKeyPath(\.label)
  }
  func testKeyPathMethod() -> Int {
    let result = invokeKeyPathMethod(\.label, method: \MyLabel.x(val:), arg: 10)
    return result
  }
}

// CHECK: label
print(Container(Controller()).test())
// CHECK: 10
print(Container(Controller()).testKeyPathMethod())

struct MetatypeContainer<V> {
  var v : V.Type
  init(_ v: V.Type) {
    self.v = v
  }
  func useMetatypeKeyPath() -> Bool {
    if let labelType = v as? MyLabel.Type {
      return labelType.isVisible
    }
    return false
  }
  func getKeyPathMethodVal() -> Int {
    if let labelType = v as? MyLabel.Type {
      return labelType.y(val: 20)
    }
    return 0
  }
  func createInstanceWithDefaultInit() -> MyLabel? {
    if let labelType = v as? MyLabel.Type {
      return labelType.init()
    }
    return nil
  }
  func createInstanceWithCustomInit(customText: String) -> MyLabel? {
    if let labelType = v as? MyLabel.Type {
      return labelType.init(customText: customText)
    }
    return nil
  }
}

// CHECK: true
print(MetatypeContainer(MyLabel.self).useMetatypeKeyPath())
// CHECK: 20
print(MetatypeContainer(MyLabel.self).getKeyPathMethodVal())
// CHECK: label
if let instance = MetatypeContainer(MyLabel.self).createInstanceWithDefaultInit() {
  print(instance.text)
}
// CHECK: Custom Label
if let customInstance = MetatypeContainer(MyLabel.self).createInstanceWithCustomInit(customText: "Custom Label") {
  print(customInstance.text)
}

public class GenericController<U> {
  init(_ u: U) {
    self.u = u
  }

  var u : U
  fileprivate let label = MyLabel()
}

public func generic_class_constrained_keypath<U, V>(_ c: V) where V : GenericController<U> {
  let kp = \V.label
  print(kp)
  print(c[keyPath: kp].text)
}

// CHECK: GenericController<Int>.label
// CHECK: label
generic_class_constrained_keypath(GenericController(5))

struct S {
  var year = 2024
  static let millenium: Int = 3
  init() {}
  init(val value: Int = 2024) { year = value }
  
  var add: (Int, Int) -> Int { return { $0 + $1 } }
  func add(this: Int) -> Int { this + this}
  func add(that: Int) -> Int { that + that }
  static func subtract(_ val: Int) -> Int { return millenium - val }
  nonisolated func nonisolatedNextYear() -> Int { year + 1 }
  consuming func consume() { print(year) }
  subscript(index: Int) -> Int { return year + index}
}

// CHECK: {{\\Controller\.secondLabel!\.text|\\Controller\.<computed 0x.* \(Optional<MyLabel>\)>!\.<computed 0x.* \(String\)>}}
print(\Controller.secondLabel!.text)

// CHECK: {{\\Controller\.subscript\(_: String\)|\\Controller\.<computed 0x.* \(String\)>}}
print(\Controller["abc"])
// CHECK: \S.year
print(\S.year)
// CHECK: {{\\Controller\.subscript\(int: Int, str: String, _: Int\)|\\Controller\.<computed 0x.* \(Int\)>}}
print(\Controller[int: 0, str: "", 0])
// CHECK: {{\\Controller\.thirdLabel|\\Controller\.<computed 0x.* \(Optional<MyLabel>\)>}}
print(\Controller.thirdLabel)
// CHECK: {{\\Controller\.subscript\(\)|\\Controller\.<computed 0x.* \(Int\)>}}
print(\Controller.[])
// CHECK: \Controller.self
print(\Controller.self)

// Subscripts with dependent generic types don't produce good output currently,
// so we're just checking to make sure they don't crash.
// CHECK: \Controller.
print(\Controller[[42]])
// CHECK: Controller.
print(\Controller[[42], [42]])
// CHECK: \Controller.
print(\Controller[array: [42]])
// CHECK: \Controller.
print(\Controller[array: [42], array2: [42]])

// CHECK: {{\\Controller\.(fourthLabel|<computed .* \(Optional<MyLabel\.Type>\)>)!\.<computed .* \(Bool\)>}}
print(\Controller.fourthLabel!.isVisible)

// CHECK: \S.Type.<computed {{.*}} (Int)>
print(\S.Type.millenium)
// CHECK: {{\\Controller\.(fifthLabel|<computed .* \(Optional<MyLabel\.Type>\)>)\?\.<computed .* \(Bool\)>?}}
print(\Controller.fifthLabel?.isVisible)
// CHECK: \Int.Type.<computed {{.*}} (Int)> 
print(\Int.Type.zero)

// CHECK: \S.Type.<computed {{.*}} (() -> S)>
print(\S.Type.init)
// CHECK: \S.Type.<computed {{.*}} (S)>
print(\S.Type.init())
// CHECK: \S.Type.<computed {{.*}} ((Int) -> S)>
print(\S.Type.init(val:))
// CHECK: \S.Type.<computed {{.*}} (S)>
print(\S.Type.init(val: 2025))
// CHECK: \S.Type.<computed {{.*}} (S)>.year
print(\S.Type.init(val: 2025).year)
// CHECK: 2024
let kp = \S.Type.init()[0]
let result = S.self[keyPath: kp]
print(result)
// CHECK: 7
let kpAdd = \S.add
let resultAdd = S()[keyPath: kpAdd](3, 4)
print(resultAdd)
// CHECK: \S.<computed {{.*}} ((Int) -> Int)>
print(\S.add(this:))
// CHECK: \S.<computed {{.*}} (Int)>
print(\S.add(that: 1))
// CHECK: \S.Type.<computed {{.*}} ((Int) -> Int)>
print(\S.Type.subtract)
// CHECK: \S.Type.<computed {{.*}} (Int)>
print(\S.Type.subtract(1))
// CHECK: \S.<computed {{.*}} (() -> Int)>
print(\S.nonisolatedNextYear)
// CHECK: \S.<computed {{.*}} (Int)>
print(\S.nonisolatedNextYear())
// CHECK: \S.Type.<computed {{.*}} (S)>.<computed {{.*}} (Int)>
print(\S.Type.init(val:2025).nonisolatedNextYear())
// CHECK: 2026
let kpMethodProperty = \S.Type.init(val:2025).nonisolatedNextYear().description
let resultMethodProperty = S.self[keyPath: kpMethodProperty]
print(resultMethodProperty)
// CHECK: \S.Type.<computed {{.*}} (S)>.<computed {{.*}} (Int)>.<computed {{.*}} (Int)>
print(\S.Type.init(val:2025).nonisolatedNextYear().signum())
// // CHECK: \S.<computed {{.*}} (())>
print(\S.consume())

// CHECK: false
print(\S.add(that: 1) == \S.add(this: 1))
// CHECK: false
print(\S.add(that: 1) == \S.add(this: 2))
// CHECK: false
print(\S.Type.init(val: 2024) == \S.Type.init(val: 2025))
// CHECK: false
print(\S.Type.init(val: 2024).nonisolatedNextYear() == \S.Type.init(val: 2025))
// CHECK: true
print(\S.Type.init(val: 2024).add(this: 1) != \S.Type.init(val: 2025))

class E: Hashable {
  static func == (lhs: E, rhs: E) -> Bool { return lhs === rhs }
  func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }
}
struct BaseType {
  func foo(hashableParam e: E) {}
}
let hashableInstance = E()
// CHECK: \BaseType.<computed {{.*}} (())>
print(\BaseType.foo(hashableParam: hashableInstance))

protocol Describable {
  func describe() -> String
}
struct C: Describable {
  var name: String
  func describe() -> String { return "\(name)" }
}
// CHECK: \C.<computed {{.*}} (() -> String)>
print(\C.describe)

// CHECK: false
print(\S.Type.init(val:2025) == \S.Type.init(val:2026))
// CHECK: false
print(\S.Type.init(val:2025).nonisolatedNextYear() == \S.Type.init(val:2026).nonisolatedNextYear())
// CHECK: true
print(\S.Type.init(val:2025).nonisolatedNextYear() == \S.Type.init(val:2025).nonisolatedNextYear())
// CHECK: false
print(\MyLabel.x(val:10) == \MyLabel.x(val:20))
// CHECK: true
print(\MyLabel.Type.y(val:10) == \MyLabel.Type.y(val:10))

do {
  struct S {
    var i: Int
  }

  func test<T, U>(v: T, _ kp: any KeyPath<T, U> & Sendable) {
    print(v[keyPath: kp])
  }

  // CHECK: 42
  test(v: S(i: 42), \.i)
}

do {
  @dynamicMemberLookup
  struct Test<T> {
    var obj: T

    subscript<U>(dynamicMember member: KeyPath<T, U> & Sendable) -> U {
      get { obj[keyPath: member] }
    }
  }

  // CHECK: 5
  print(Test(obj: "Hello").utf8.count)
}

do {
  struct S1: Hashable {}
  struct S2 {
    subscript(param: S1) -> String { "Subscript with private type" }
  }

  let kp = \S2[S1()]
  // CHECK: Subscript with private type
  print(S2()[keyPath: kp])
  // CHECK: {{\\S2\.subscript\(_: S1 #[0-9]+\)|\S2\.<computed 0x.* \(String\)>}}
  print(kp)
}

do {
  struct Weekday {
      static let day = "Monday"
  }
  
  @dynamicMemberLookup
  struct StaticExample<T> {
      subscript<U>(dynamicMember keyPath: KeyPath<T.Type, U>) -> U {
          return T.self[keyPath: keyPath]
      }
  }
  // CHECK: true
  print(StaticExample<MyLabel>().isVisible)
}

do {
  @dynamicMemberLookup
  struct InstanceDynamicMemberLookup<T> {
      var obj: T
      
      subscript<U>(dynamicMember member: KeyPath<T, (Int) -> U>) -> (Int) -> U {
          get { obj[keyPath: member] }
      }
  }

  // CHECK: 50
  let instanceDynamicLookup = InstanceDynamicMemberLookup(obj: MyLabel())
  print(instanceDynamicLookup.x(50))
}

extension MyLabel {
  static var defaultInitializer: () -> MyLabel { return MyLabel.init }
  static var customInitializer: (String) -> MyLabel { return MyLabel.init(customText:) }
}

do {
  @dynamicMemberLookup
  struct StaticDynamicMemberLookup<T> {
    subscript<U>(dynamicMember keyPath: KeyPath<T.Type, (Int) -> U>) -> (Int) -> U {
        return T.self[keyPath: keyPath]
    }
    subscript<U>(dynamicMember keyPath: KeyPath<T.Type, () -> U>) -> () -> U {
      return T.self[keyPath: keyPath]
    }
    subscript<U>(dynamicMember keyPath: KeyPath<T.Type, (String) -> U>) -> (String) -> U {
        return T.self[keyPath: keyPath]
    }
  }
  
  // CHECK: 60
  let staticDynamicLookup = StaticDynamicMemberLookup<MyLabel>()
  print(staticDynamicLookup.y(60))
  // CHECK: label
  let defaultInstance = staticDynamicLookup.defaultInitializer()
  print(defaultInstance.text)
  // CHECK: Custom Label
  let customInstance = staticDynamicLookup.customInitializer("Custom Label")
  print(customInstance.text)
}
