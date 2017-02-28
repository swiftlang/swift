// Note: test of the scope map. All of these tests are line- and
// column-sensitive, so any additions should go at the end.

struct S0 {
  class InnerC0 { }
}

extension S0 {
}

class C0 {
}

enum E0 {
  case C0
  case C1(Int, Int)
}

struct GenericS0<T, U> {
}

func genericFunc0<T, U>(t: T, u: U, i: Int = 10) {
}

class ContainsGenerics0 {
  init<T, U>(t: T, u: U) {
  }

  deinit {
  }
}

typealias GenericAlias0<T> = [T]

#if arch(unknown)
struct UnknownArchStruct { }
#else
struct OtherArchStruct { }
#endif

func functionBodies1(a: Int, b: Int?) {
  let (x1, x2) = (a, b),
      (y1, y2) = (b, a)
  let (z1, z2) = (a, a)
  do {
    let a1 = a
    let a2 = a
    do {
      let b1 = b
      let b2 = b
    }
  }
  do {
    let b1 = b
    let b2 = b
  }
  func f(_ i: Int) -> Int { return i }
  let f2 = f(_:)
  struct S7 { }
  typealias S7Alias = S7
  
  if let b1 = b, let b2 = b {
    let c1 = b
  } else {
    let c2 = b
  }

  guard let b1 = b, { $0 > 5 }(b1), let b2 = b else {
    let c = 5
    return
  }

  while let b3 = b, let b4 = b {
    let c = 5
  }

  repeat { } while true;

  for (x, y) in [(1, "hello"), (2, "world")] where x % 2 == 0 {

  }

  do {
    try throwing()
  } catch let mine as MyError where mine.value == 17 {
  } catch {
  }

  switch MyEnum.second(1) {
  case .second(let x) where x == 17:
    break;

  case .first:
    break;

  default:
    break;
  }
  for (var i = 0; i != 10; i += 1) { }
}

func throwing() throws { }

struct MyError : Error {
  var value: Int
}

enum MyEnum {
  case first
  case second(Int)
  case third
}

struct StructContainsAbstractStorageDecls {
  subscript (i: Int, j: Int) -> Int {
    set {
    }
    get {
      return i + j
    }
  }

  var computed: Int {
    get {
      return 0
    }
    set {
    }
  }
}

class ClassWithComputedProperties {
  var willSetProperty: Int = 0 {
    willSet { }
  } 

  var didSetProperty: Int = 0 {
    didSet { }
  } 
}

func funcWithComputedProperties(i: Int) {
  var computed: Int {
    set {
    }
    get {
      return 0
    }
  }, var (stored1, stored2) = (1, 2),
  var alsoComputed: Int {
    return 17
  }
    
  do { }
}

func closures() {
  { x, y in 
    return { $0 + $1 }(x, y)
  }(1, 2) + 
  { a, b in a * b }(3, 4)
}

{ closures() }()

func defaultArguments(i: Int = 1,
                      j: Int = { $0 + $1 }(1, 2)) {

  func localWithDefaults(i: Int = 1,
                         j: Int = { $0 + $1 }(1, 2)) {
  }

  let a = i + j
  { $0 }(a)
}

struct PatternInitializers {
  var (a, b) = (1, 2),
      (c, d) = (1.5, 2.5)
}

protocol ProtoWithSubscript {
  subscript(native: Int) -> Int { get set }
}

func localPatternsWithSharedType() {
  let i, j, k: Int
}

class LazyProperties {
  var value: Int = 17

  lazy var prop: Int = self.value
}

// RUN: not %target-swift-frontend -dump-scope-maps expanded %s 2> %t.expanded
// RUN: %FileCheck -check-prefix CHECK-EXPANDED %s < %t.expanded

// CHECK-EXPANDED: SourceFile{{.*}}scope_map.swift{{.*}}expanded
// CHECK-EXPANDED-NEXT: TypeDecl {{.*}} S0 [4:1 - 6:1] expanded
// CHECK-EXPANDED-NEXT: TypeOrExtensionBody {{.*}} 'S0' [4:11 - 6:1] expanded
// CHECK-EXPANDED-NEXT: -TypeDecl {{.*}} InnerC0 [5:3 - 5:19] expanded
// CHECK-EXPANDED-NEXT: `-TypeOrExtensionBody {{.*}} 'InnerC0' [5:17 - 5:19] expanded
// CHECK-EXPANDED-NEXT: -ExtensionGenericParams {{.*}} extension of 'S0' [8:14 - 9:1] expanded
// CHECK-EXPANDED-NEXT: -TypeOrExtensionBody {{.*}} extension of 'S0' [8:14 - 9:1] expanded
// CHECK-EXPANDED-NEXT: TypeDecl {{.*}} C0 [11:1 - 12:1] expanded
// CHECK-EXPANDED-NEXT: -TypeOrExtensionBody {{.*}} 'C0' [11:10 - 12:1] expanded
// CHECK-EXPANDED-NEXT: TypeDecl {{.*}} E0 [14:1 - 17:1] expanded
// CHECK-EXPANDED-NEXT: -TypeOrExtensionBody {{.*}} 'E0' [14:9 - 17:1] expanded
// CHECK-EXPANDED-NEXT: TypeDecl {{.*}} GenericS0 [19:1 - 20:1] expanded
// CHECK-EXPANDED-NEXT: -GenericParams {{.*}} param 0 [19:18 - 20:1] expanded
// CHECK-EXPANDED-NEXT:   -GenericParams {{.*}} param 1 [19:21 - 20:1] expanded
// CHECK-EXPANDED-NEXT:     -TypeOrExtensionBody {{.*}} 'GenericS0' [19:24 - 20:1] expanded
// CHECK-EXPANDED-NEXT:-AbstractFunctionDecl {{.*}} genericFunc0(t:u:i:) [22:1 - 23:1] expanded
// CHECK-EXPANDED-NEXT:  -GenericParams {{.*}} param 0 [22:19 - 23:1] expanded
// CHECK-EXPANDED-NEXT:   -GenericParams {{.*}} param 1 [22:22 - 23:1] expanded
// CHECK-EXPANDED-NEXT:   -AbstractFunctionParams {{.*}} genericFunc0(t:u:i:) param 0:0 [22:28 - 23:1] expanded
// CHECK-EXPANDED-NEXT:     -AbstractFunctionParams {{.*}} genericFunc0(t:u:i:) param 0:1 [22:34 - 23:1] expanded
// CHECK-EXPANDED:            |-DefaultArgument {{.*}} [22:46 - 22:46] expanded
// CHECK-EXPANDED:            `-AbstractFunctionParams {{.*}} genericFunc0(t:u:i:) param 0:2 [22:46 - 23:1] expanded
// CHECK-EXPANDED-NEXT:         `-AbstractFunctionBody {{.*}} genericFunc0(t:u:i:) [22:50 - 23:1] expanded
// CHECK-EXPANDED-NEXT:         -BraceStmt {{.*}} [22:50 - 23:1] expanded
// CHECK-EXPANDED-NEXT: TypeDecl {{.*}} ContainsGenerics0 [25:1 - 31:1] expanded
// CHECK-EXPANDED-NEXT: -TypeOrExtensionBody {{.*}} 'ContainsGenerics0' [25:25 - 31:1] expanded
// CHECK-EXPANDED-NEXT:  -AbstractFunctionDecl {{.*}} init(t:u:) [26:3 - 27:3] expanded
// CHECK-EXPANDED-NEXT:   -GenericParams {{.*}} param 0 [26:8 - 27:3] expanded
// CHECK-EXPANDED-NEXT:     -GenericParams {{.*}} param 1 [26:11 - 27:3] expanded
// CHECK-EXPANDED-NEXT:     -AbstractFunctionParams {{.*}} init(t:u:) param 0:0 [26:13 - 27:3] expanded
// CHECK-EXPANDED-NEXT:       -AbstractFunctionParams {{.*}} init(t:u:) param 1:0 [26:17 - 27:3] expanded
// CHECK-EXPANDED-NEXT:         -AbstractFunctionParams {{.*}} init(t:u:) param 1:1 [26:23 - 27:3] expanded
// CHECK-EXPANDED-NEXT:           `-AbstractFunctionBody {{.*}} init(t:u:) [26:26 - 27:3] expanded
// CHECK-EXPANDED-NEXT:           -BraceStmt {{.*}} [26:26 - 27:3] expanded
// CHECK-EXPANDED-NEXT: -AbstractFunctionDecl {{.*}} deinit
// CHECK-EXPANDED-NEXT:   -AbstractFunctionParams {{.*}} deinit param 0:0 [29:3 - 30:3] expanded
// CHECK-EXPANDED-NEXT:     `-AbstractFunctionBody {{.*}} deinit [29:10 - 30:3] expanded
// CHECK-EXPANDED-NEXT:     -BraceStmt {{.*}} [29:10 - 30:3] expanded
// CHECK-EXPANDED-NEXT: TypeDecl {{.*}} GenericAlias0 [33:1 - 33:32] expanded
// CHECK-EXPANDED-NEXT: -GenericParams {{.*}} param 0 [33:25 - 33:32] expanded
// CHECK-EXPANDED-NEXT: TypeDecl {{.*}} {{.*}}ArchStruct [{{.*}}] expanded
// CHECK-EXPANDED-NEXT: TypeOrExtensionBody {{.*}} '{{.*}}ArchStruct' [{{.*}}] expanded
// CHECK-EXPANDED-NEXT: {{^}}|-AbstractFunctionDecl {{.*}} functionBodies1(a:b:) [41:1 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}  `-AbstractFunctionParams {{.*}} functionBodies1(a:b:) param 0:0 [41:25 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}  `-AbstractFunctionParams {{.*}} functionBodies1(a:b:) param 0:1 [41:36 - 100:1] expanded
// CHECK-EXPANDED-NEXT:           `-AbstractFunctionBody {{.*}} functionBodies1(a:b:) [41:39 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}    `-BraceStmt {{.*}} [41:39 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}      `-PatternBinding {{.*}} entry 0 [42:7 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}      |-PatternInitializer {{.*}} entry 0 [42:18 - 42:23] expanded
// CHECK-EXPANDED-NEXT: {{^}}      `-AfterPatternBinding {{.*}} entry 0 [42:23 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}        `-PatternBinding {{.*}} entry 1 [43:7 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}              |-PatternInitializer {{.*}} entry 1 [43:18 - 43:23] expanded
// CHECK-EXPANDED-NEXT: {{^}}        `-AfterPatternBinding {{.*}} entry 1 [43:23 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}          `-PatternBinding {{.*}} entry 0 [44:7 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}          |-PatternInitializer {{.*}} entry 0 [44:18 - 44:23] expanded
// CHECK-EXPANDED-NEXT: {{^}}          `-AfterPatternBinding {{.*}} entry 0 [44:23 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}            |-BraceStmt {{.*}} [45:6 - 52:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}              `-PatternBinding {{.*}} entry 0 [46:9 - 52:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                |-PatternInitializer {{.*}} entry 0 [46:14 - 46:14] expanded
// CHECK-EXPANDED-NEXT: {{^}}              `-AfterPatternBinding {{.*}} entry 0 [46:14 - 52:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                `-PatternBinding {{.*}} entry 0 [47:9 - 52:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                  |-PatternInitializer {{.*}} entry 0 [47:14 - 47:14] expanded
// CHECK-EXPANDED-NEXT: {{^}}                `-AfterPatternBinding {{.*}} entry 0 [47:14 - 52:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                  `-BraceStmt {{.*}} [48:8 - 51:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}                    `-PatternBinding {{.*}} entry 0 [49:11 - 51:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}                      |-PatternInitializer {{.*}} entry 0 [49:16 - 49:16] expanded
// CHECK-EXPANDED-NEXT: {{^}}                    `-AfterPatternBinding {{.*}} entry 0 [49:16 - 51:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}                      `-PatternBinding {{.*}} entry 0 [50:11 - 51:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}                      |-PatternInitializer {{.*}} entry 0 [50:16 - 50:16] expanded
// CHECK-EXPANDED-NEXT: {{^}}                      `-AfterPatternBinding {{.*}} entry 0 [50:16 - 51:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}            |-BraceStmt {{.*}} [53:6 - 56:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}              `-PatternBinding {{.*}} entry 0 [54:9 - 56:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                |-PatternInitializer {{.*}} entry 0 [54:14 - 54:14] expanded
// CHECK-EXPANDED-NEXT: {{^}}              `-AfterPatternBinding {{.*}} entry 0 [54:14 - 56:3] expanded
// CHECK-EXPANDED: {{^}}                `-AfterPatternBinding {{.*}} entry 0 [55:14 - 56:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}              |-AbstractFunctionDecl {{.*}} f(_:) [57:3 - 57:38] expanded
// CHECK-EXPANDED-NEXT: {{^}}                `-AbstractFunctionParams {{.*}} f(_:) param 0:0 [57:15 - 57:38] expanded
// CHECK-EXPANDED-NEXT:                       `-AbstractFunctionBody {{.*}} f(_:) [57:27 - 57:38] expanded
// CHECK-EXPANDED-NEXT: {{^}}                  `-BraceStmt {{.*}} [57:27 - 57:38] expanded
// CHECK-EXPANDED: {{^}}              `-AfterPatternBinding {{.*}} entry 0 [58:16 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}                |-TypeDecl {{.*}} S7 [59:3 - 59:15] expanded
// CHECK-EXPANDED-NEXT: {{^}}                  `-TypeOrExtensionBody {{.*}} 'S7' [59:13 - 59:15] expanded
// CHECK-EXPANDED-NEXT: {{^}}                  |-TypeDecl {{.*}} S7Alias [60:3 - 60:23] expanded
// CHECK-EXPANDED-NEXT: {{^}}                    |-IfStmt {{.*}} [62:3 - 66:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                      |-ConditionalClause {{.*}} index 0 [62:18 - 64:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                        `-ConditionalClause {{.*}} index 1 [62:29 - 64:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          `-BraceStmt {{.*}} [62:29 - 64:3] expanded
// CHECK-EXPANDED: {{^}}                                 `-AfterPatternBinding {{.*}} entry 0 [63:14 - 64:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                      `-BraceStmt {{.*}} [64:10 - 66:3] expanded
// CHECK-EXPANDED: {{^}}                             `-AfterPatternBinding {{.*}} entry 0 [65:14 - 66:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                    `-GuardStmt {{.*}} [68:3 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}                      |-ConditionalClause {{.*}} index 0 [68:21 - 68:53] expanded
// CHECK-EXPANDED-NEXT: {{^}}                        `-ConditionalClause {{.*}} index 1 [68:21 - 68:53] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          |-Closure {{.*}} [68:21 - 68:30] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            `-BraceStmt {{.*}} [68:21 - 68:30] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          `-ConditionalClause {{.*}} index 2 [68:53 - 68:53] expanded
// CHECK-EXPANDED-NEXT: {{^}}                      |-BraceStmt {{.*}} [68:53 - 71:3] expanded
// CHECK-EXPANDED: {{^}}                        `-AfterPatternBinding {{.*}} entry 0 [69:13 - 71:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                      `-ConditionalClause {{.*}} index 0 guard-continuation [71:3 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}                        `-ConditionalClause {{.*}} index 1 guard-continuation [71:3 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          `-ConditionalClause {{.*}} index 2 guard-continuation [71:3 - 100:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          |-ConditionalClause {{.*}} index 0 [73:21 - 75:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            `-ConditionalClause {{.*}} index 1 [73:32 - 75:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                              `-BraceStmt {{.*}} [73:32 - 75:3] expanded
// CHECK-EXPANDED: {{^}}                                `-AfterPatternBinding {{.*}} entry 0 [74:13 - 75:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          |-RepeatWhileStmt {{.*}} [77:3 - 77:20] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            `-BraceStmt {{.*}} [77:10 - 77:12] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          |-ForEachStmt {{.*}} [79:3 - 81:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            `-ForEachPattern {{.*}} [79:52 - 81:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                              `-BraceStmt {{.*}} [79:63 - 81:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          |-DoCatchStmt {{.*}} [83:3 - 87:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            |-BraceStmt {{.*}} [83:6 - 85:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            |-CatchStmt {{.*}} [85:31 - 86:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                              `-BraceStmt {{.*}} [85:54 - 86:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            `-CatchStmt {{.*}} [86:11 - 87:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                              `-BraceStmt {{.*}} [86:11 - 87:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          |-SwitchStmt {{.*}} [89:3 - 98:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            |-CaseStmt {{.*}} [90:29 - 91:10] expanded
// CHECK-EXPANDED-NEXT: {{^}}                              `-BraceStmt {{.*}} [91:5 - 91:10] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            |-CaseStmt {{.*}} [94:5 - 94:10] expanded
// CHECK-EXPANDED-NEXT: {{^}}                              `-BraceStmt {{.*}} [94:5 - 94:10] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            `-CaseStmt {{.*}} [97:5 - 97:10] expanded
// CHECK-EXPANDED-NEXT: {{^}}                              `-BraceStmt {{.*}} [97:5 - 97:10] expanded
// CHECK-EXPANDED-NEXT: {{^}}                          `-ForStmt {{.*}} [99:3 - 99:38] expanded
// CHECK-EXPANDED-NEXT: {{^}}                            `-ForStmtInitializer {{.*}} [99:17 - 99:38] expanded
// CHECK-EXPANDED-NEXT: {{^}}                              `-BraceStmt {{.*}} [99:36 - 99:38] expanded

// CHECK-EXPANDED:        TypeDecl {{.*}} StructContainsAbstractStorageDecls [114:1 - 130:1] expanded
// CHECK-EXPANDED-NEXT: `-TypeOrExtensionBody {{.*}} 'StructContainsAbstractStorageDecls' [114:43 - 130:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}  |-Accessors {{.*}} scope_map.(file).StructContainsAbstractStorageDecls.subscript@{{.*}}scope_map.swift:115:3 [115:37 - 121:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}  |-AbstractFunctionDecl {{.*}} _ [116:5 - 117:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}    `-AbstractFunctionParams {{.*}} _ param 0:0 [116:5 - 117:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}      `-AbstractFunctionParams {{.*}} _ param 1:0 [116:5 - 117:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}        `-AbstractFunctionParams {{.*}} _ param 1:1 [116:5 - 117:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}          `-AbstractFunctionParams {{.*}} _ param 1:2 [116:5 - 117:5] expanded
// CHECK-EXPANDED-NEXT:                  `-AbstractFunctionBody {{.*}} _ [116:9 - 117:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}            `-BraceStmt {{.*}} [116:9 - 117:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}  `-AbstractFunctionDecl {{.*}} _ [118:5 - 120:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}    `-AbstractFunctionParams {{.*}} _ param 0:0 [118:5 - 120:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}      `-AbstractFunctionParams {{.*}} _ param 1:0 [118:5 - 120:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}        `-AbstractFunctionParams {{.*}} _ param 1:1 [118:5 - 120:5] expanded
// CHECK-EXPANDED-NEXT:                `-AbstractFunctionBody {{.*}} _ [118:9 - 120:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}          `-BraceStmt {{.*}} [118:9 - 120:5] expanded

// CHECK-EXPANDED: {{^}}  `-Accessors {{.*}} scope_map.(file).StructContainsAbstractStorageDecls.computed@{{.*}}scope_map.swift:123:7 [123:21 - 129:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}  |-AbstractFunctionDecl {{.*}} _ [124:5 - 126:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}    `-AbstractFunctionParams {{.*}} _ param 0:0 [124:5 - 126:5] expanded
// CHECK-EXPANDED-NEXT:            `-AbstractFunctionBody {{.*}} _ [124:9 - 126:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}      `-BraceStmt {{.*}} [124:9 - 126:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}    `-AbstractFunctionDecl {{.*}} _ [127:5 - 128:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}    `-AbstractFunctionParams {{.*}} _ param 0:0 [127:5 - 128:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}      `-AbstractFunctionParams {{.*}} _ param 1:0 [127:5 - 128:5] expanded
// CHECK-EXPANDED-NEXT:              `-AbstractFunctionBody {{.*}} _ [127:9 - 128:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}        `-BraceStmt {{.*}} [127:9 - 128:5] expanded

// CHECK-EXPANDED:  TypeDecl {{.*}} ClassWithComputedProperties [132:1 - 140:1] expanded
// CHECK-EXPANDED-NEXT: -TypeOrExtensionBody {{.*}} 'ClassWithComputedProperties' [132:35 - 140:1] expanded
// CHECK-EXPANDED: {{^}}  `-Accessors {{.*}} scope_map.(file).ClassWithComputedProperties.willSetProperty@{{.*}}scope_map.swift:133:7 [133:32 - 135:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}   `-AbstractFunctionDecl {{.*}} _ [134:5 - 134:15] expanded
// CHECK-EXPANDED-NEXT: {{^}}    `-AbstractFunctionParams {{.*}} _ param 0:0 [134:5 - 134:15] expanded
// CHECK-EXPANDED-NEXT: {{^}}      `-AbstractFunctionParams {{.*}} _ param 1:0 [134:5 - 134:15] expanded
// CHECK-EXPANDED: {{^}}        `-BraceStmt {{.*}} [134:13 - 134:15] expanded
// CHECK-EXPANDED: {{^}}       `-Accessors {{.*}} scope_map.(file).ClassWithComputedProperties.didSetProperty@{{.*}}scope_map.swift:137:7 [137:31 - 139:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}    `-AbstractFunctionDecl {{.*}} _ [138:5 - 138:14] expanded
// CHECK-EXPANDED-NEXT: {{^}}    `-AbstractFunctionParams {{.*}} _ param 0:0 [138:5 - 138:14] expanded
// CHECK-EXPANDED-NEXT: {{^}}      `-AbstractFunctionParams {{.*}} _ param 1:0 [138:5 - 138:14] expanded
// CHECK-EXPANDED: {{^}}        `-BraceStmt {{.*}} [138:12 - 138:14] expanded

// CHECK-EXPANDED: {{^}}  `-AbstractFunctionParams {{.*}} funcWithComputedProperties(i:) param 0:0 [142:36 - 155:1] expanded
// CHECK-EXPANDED: {{^}}  `-BraceStmt {{.*}} [142:41 - 155:1] expanded
// CHECK-EXPANDED-NEXT: {{^}} `-PatternBinding {{.*}} entry 0 [143:7 - 155:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}    `-AfterPatternBinding {{.*}} entry 0 [143:17 - 155:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}      |-Accessors {{.*}} scope_map.(file).funcWithComputedProperties(i:).computed@{{.*}}scope_map.swift:143:7 [143:21 - 149:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}       |-AbstractFunctionDecl {{.*}} _ [144:5 - 145:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}        `-AbstractFunctionParams {{.*}} _ param 0:0 [144:5 - 145:5] expanded
// CHECK-EXPANDED: {{^}}          `-BraceStmt {{.*}} [144:9 - 145:5] expanded
// CHECK-EXPANDED-NEXT: {{^}}    `-AbstractFunctionDecl {{.*}} _ [146:5 - 148:5] expanded
// CHECK-EXPANDED: {{^}}        `-BraceStmt {{.*}} [146:9 - 148:5] expanded
// CHECK-EXPANDED: {{^}}      `-AfterPatternBinding {{.*}} entry 1 [149:36 - 155:1] expanded
// CHECK-EXPANDED: {{^}}        `-AfterPatternBinding {{.*}} entry 2 [150:21 - 155:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}           |-AbstractFunctionDecl {{.*}} _ [150:25 - 152:3] expanded
// CHECK-EXPANDED: {{^}}            `-BraceStmt {{.*}} [150:25 - 152:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}              `-BraceStmt {{.*}} [154:6 - 154:8] expanded

// CHECK-EXPANDED: |-AbstractFunctionDecl {{.*}} closures() [157:1 - 162:1] expanded
// CHECK-EXPANDED: {{^}}  `-BraceStmt {{.*}} [157:17 - 162:1] expanded
// CHECK-EXPANDED-NEXT: {{^}}  `-Preexpanded {{.*}} [158:10 - 161:19] expanded
// CHECK-EXPANDED-NEXT: {{^}}    |-Closure {{.*}} [158:10 - 160:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}      `-BraceStmt {{.*}} [158:10 - 160:3] expanded
// CHECK-EXPANDED-NEXT: {{^}}        `-Closure {{.*}} [159:12 - 159:22] expanded
// CHECK-EXPANDED-NEXT: {{^}}          `-BraceStmt {{.*}} [159:12 - 159:22] expanded
// CHECK-EXPANDED-NEXT: {{^}}    `-Closure {{.*}} [161:10 - 161:19] expanded
// CHECK-EXPANDED-NEXT: {{^}}      `-BraceStmt {{.*}} [161:10 - 161:19] expanded

// CHECK-EXPANDED: `-TopLevelCode {{.*}} [164:1 - [[EOF:[0-9]+:[0-9]+]]] expanded
// CHECK-EXPANDED-NEXT: {{^}}  `-BraceStmt {{.*}} [164:1 - [[EOF]]] expanded
// CHECK-EXPANDED-NEXT: {{^}}    |-Closure {{.*}} [164:1 - 164:14] expanded
// CHECK-EXPANDED-NEXT: {{^}}      `-BraceStmt {{.*}} [164:1 - 164:14] expanded

// CHECK-EXPANDED: -AbstractFunctionDecl {{.*}} defaultArguments(i:j:) [166:1 - 175:1] expanded
// CHECK-EXPANDED: {{^}}    |-DefaultArgument {{.*}} [166:32 - 166:32] expanded
// CHECK-EXPANDED-NEXT: {{^}}    `-AbstractFunctionParams {{.*}} defaultArguments(i:j:) param 0:0 [166:32 - 175:1] expanded
// CHECK-EXPANDED: {{^}}        |-DefaultArgument {{.*}} [167:32 - 167:48] expanded
// CHECK-EXPANDED-NEXT: {{^}}          `-Closure {{.*}} [167:32 - 167:42] expanded
// CHECK-EXPANDED-NEXT: {{^}}            `-BraceStmt {{.*}} [167:32 - 167:42] expanded
// CHECK-EXPANDED-NEXT: {{^}}        `-AbstractFunctionParams {{.*}} defaultArguments(i:j:) param 0:1 [167:48 - 175:1] expanded

// CHECK-EXPANDED: -Accessors {{.*}} scope_map.(file).ProtoWithSubscript.subscript@{{.*}}scope_map.swift:183:3 [183:33 - 183:43] expanded
// CHECK-EXPANDED-NEXT:     |-AbstractFunctionDecl {{.*}} _ [183:35 - 183:35] expanded
// CHECK-EXPANDED-NEXT:       `-AbstractFunctionParams {{.*}} _ param 0:0 [183:35 - 183:35] expanded
// CHECK-EXPANDED-NEXT:         `-AbstractFunctionParams {{.*}} _ param 1:0 [183:35 - 183:35] expanded
// CHECK-EXPANDED-NEXT:     `-AbstractFunctionDecl {{.*}} _ [183:39 - 183:39] expanded
// CHECK-EXPANDED-NEXT:       `-AbstractFunctionParams {{.*}} _ param 0:0 [183:39 - 183:39] expanded
// CHECK-EXPANDED-NEXT:         `-AbstractFunctionParams {{.*}} _ param 1:0 [183:39 - 183:39] expanded
// CHECK-EXPANDED-NEXT:           `-AbstractFunctionParams {{.*}} _ param 1:1 [183:39 - 183:39] expanded

// CHECK-EXPANDED: -AbstractFunctionDecl {{.*}} localPatternsWithSharedType() [186:1 - 188:1] expanded
// CHECK-EXPANDED:  `-BraceStmt {{.*}} [186:36 - 188:1] expanded
// CHECK-EXPANDED-NEXT:    `-PatternBinding {{.*}} entry 0 [187:7 - 188:1] expanded
// CHECK-EXPANDED-NEXT:      `-AfterPatternBinding {{.*}} entry 0 [187:7 - 188:1] expanded
// CHECK-EXPANDED-NEXT:        `-PatternBinding {{.*}} entry 1 [187:10 - 188:1] expanded
// CHECK-EXPANDED-NEXT:          `-AfterPatternBinding {{.*}} entry 1 [187:10 - 188:1] expanded
// CHECK-EXPANDED-NEXT:            `-PatternBinding {{.*}} entry 2 [187:13 - 188:1] expanded
// CHECK-EXPANDED-NEXT:              `-AfterPatternBinding {{.*}} entry 2 [187:16 - 188:1] expanded

// RUN: not %target-swift-frontend -dump-scope-maps 70:8,26:20,5:18,166:32,179:18,193:26 %s 2> %t.searches
// RUN: %FileCheck -check-prefix CHECK-SEARCHES %s < %t.searches

// CHECK-SEARCHES-LABEL: ***Scope at 70:8***
// CHECK-SEARCHES-NEXT: AfterPatternBinding {{.*}} entry 0 [69:13 - 71:3] expanded
// CHECK-SEARCHES-NEXT: Local bindings: c

// CHECK-SEARCHES-LABEL: ***Scope at 26:20***
// CHECK-SEARCHES-NEXT: AbstractFunctionParams {{.*}} init(t:u:) param 1:0 [26:17 - 27:3] expanded
// CHECK-SEARCHES-NEXT: Local bindings: t

// CHECK-SEARCHES-LABEL: ***Scope at 5:18***
// CHECK-SEARCHES-NEXT: TypeOrExtensionBody {{.*}} 'InnerC0' [5:17 - 5:19] expanded
// CHECK-SEARCHES-NEXT: Module name=scope_map
// CHECK-SEARCHES-NEXT:   FileUnit file="{{.*}}scope_map.swift"
// CHECK-SEARCHES-NEXT:     StructDecl name=S0
// CHECK-SEARCHES-NEXT:       ClassDecl name=InnerC0

// CHECK-SEARCHES-LABEL: ***Scope at 166:32***
// CHECK-SEARCHES-NEXT: DefaultArgument {{.*}} [166:32 - 166:32] expanded
// CHECK-SEARCHES-NEXT: Module name=scope_map
// CHECK-SEARCHES-NEXT:   FileUnit file="{{.*}}scope_map.swift"
// CHECK-SEARCHES-NEXT:     AbstractFunctionDecl name=defaultArguments : (Int, Int) -> ()
// CHECK-SEARCHES-NEXT:       {{.*}} Initializer DefaultArgument index=0

// CHECK-SEARCHES-LABEL: ***Scope at 179:18***
// CHECK-SEARCHES-NEXT: PatternInitializer {{.*}} entry 1 [179:16 - 179:25] expanded
// CHECK-SEARCHES-NEXT: {{.*}} Module name=scope_map
// CHECK-SEARCHES-NEXT:   {{.*}} FileUnit file="{{.*}}scope_map.swift"
// CHECK-SEARCHES-NEXT:     {{.*}} StructDecl name=PatternInitializers
// CHECK-SEARCHES-NEXT:       {{.*}} Initializer PatternBinding {{.*}} #1

// CHECK-SEARCHES-LABEL: ***Scope at 193:26***
// CHECK-SEARCHES-NEXT: PatternInitializer {{.*}} entry 0 [193:24 - 193:29] expanded
// CHECK-SEARCHES-NEXT: name=scope_map
// CHECK-SEARCHES-NEXT:   FileUnit file="{{.*}}scope_map.swift"
// CHECK-SEARCHES-NEXT:     ClassDecl name=LazyProperties
// CHECK-SEARCHES-NEXT:       Initializer PatternBinding {{.*}} #0

// FIXME: Re-enable the binding below
// CHECK-SEARCHES-NOT: Local bindings: self

// CHECK-SEARCHES-LABEL: ***Complete scope map***
// CHECK-SEARCHES-NEXT: SourceFile {{.*}} '{{.*}}scope_map.swift' [1:1 - [[EOF:[0-9]+:[0-9]+]]] unexpanded
// CHECK-SEARCHES: TypeOrExtensionBody {{.*}} 'S0' [4:11 - 6:1] expanded
// CHECK-SEARCHES: -TypeOrExtensionBody {{.*}} 'InnerC0' [5:17 - 5:19] expanded
// CHECK-SEARCHES-NOT: {{ expanded}}
// CHECK-SEARCHES: -TypeDecl {{.*}} ContainsGenerics0 [25:1 - 31:1] expanded
// CHECK-SEARCHES-NEXT: `-TypeOrExtensionBody {{.*}} 'ContainsGenerics0' [25:25 - 31:1] expanded
// CHECK-SEARCHES-NEXT:   |-AbstractFunctionDecl {{.*}} init(t:u:) [26:3 - 27:3] expanded
// CHECK-SEARCHES-NEXT:   `-GenericParams {{.*}} param 0 [26:8 - 27:3] expanded
// CHECK-SEARCHES-NEXT:     `-GenericParams {{.*}} param 1 [26:11 - 27:3] expanded
// CHECK-SEARCHES-NEXT:       `-AbstractFunctionParams {{.*}} init(t:u:) param 0:0 [26:13 - 27:3] expanded
// CHECK-SEARCHES-NEXT:         `-AbstractFunctionParams {{.*}} init(t:u:) param 1:0 [26:17 - 27:3] expanded
// CHECK-SEARCHES-NEXT:           `-AbstractFunctionParams {{.*}} init(t:u:) param 1:1 [26:23 - 27:3] unexpanded
// CHECK-SEARCHES-NOT: {{ expanded}}
// CHECK-SEARCHES: |-AbstractFunctionDecl {{.*}} functionBodies1(a:b:) [41:1 - 100:1] expanded
// CHECK-SEARCHES: `-AbstractFunctionParams {{.*}} functionBodies1(a:b:) param 0:0 [41:25 - 100:1] expanded
// CHECK-SEARCHES: |-AbstractFunctionDecl {{.*}} throwing() [102:1 - 102:26] unexpanded
// CHECK-SEARCHES: -AbstractFunctionDecl {{.*}} defaultArguments(i:j:) [166:1 - 175:1] expanded
// CHECK-SEARCHES: DefaultArgument {{.*}} [166:32 - 166:32] expanded
// CHECK-SEARCHES-NOT: {{ expanded}}
// CHECK-SEARCHES: |-TypeDecl {{.*}} PatternInitializers [177:1 - 180:1] expanded
// CHECK-SEARCHES: -TypeOrExtensionBody {{.*}} 'PatternInitializers' [177:28 - 180:1] expanded
// CHECK-SEARCHES:    |-PatternBinding {{.*}} entry 0 [178:7 - 178:21] unexpanded
// CHECK-SEARCHES:    `-PatternBinding {{.*}} entry 1 [179:7 - 179:25] expanded
// CHECK-SEARCHES:      `-PatternInitializer {{.*}} entry 1 [179:16 - 179:25] expanded
// CHECK-SEARCHES-NOT: {{ expanded}}
// CHECK-SEARCHES:    |-TypeDecl {{.*}} ProtoWithSubscript [182:1 - 184:1] unexpanded
// CHECK-SEARCHES-NOT: {{ expanded}}
// CHECK-SEARCHES: |-AbstractFunctionDecl {{.*}} localPatternsWithSharedType() [186:1 - 188:1] unexpanded
// CHECK-SEARCHES: `-TypeDecl {{.*}} LazyProperties [190:1 - 194:1] expanded
// CHECK-SEARCHES: -TypeOrExtensionBody {{.*}} 'LazyProperties' [190:22 - 194:1] expanded
// CHECK-SEARCHES-NEXT:   |-PatternBinding {{.*}} entry 0 [191:7 - 191:20] unexpanded
// CHECK-SEARCHES-NEXT:   `-PatternBinding {{.*}} entry 0 [193:12 - 193:29] expanded
// CHECK-SEARCHES-NEXT:     `-PatternInitializer {{.*}} entry 0 [193:24 - 193:29] expanded
// CHECK-SEARCHES-NOT: {{ expanded}}
