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


// CHECK-EXPANDED:      ***Complete scope map***
// CHECK-EXPANDED-NEXT: ASTSourceFileScope {{.*}} 'SOURCE_DIR/test/NameBinding/scope_map.swift' [1:1 - 5{{[0-9][0-9]}}:1]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}} 'S0' [4:1 - 6:1]
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}} 'S0' [4:11 - 6:1]
// CHECK-EXPANDED-NEXT:     `-NominalTypeDeclScope {{.*}} 'InnerC0' [5:3 - 5:19]
// CHECK-EXPANDED-NEXT:       `-NominalTypeBodyScope {{.*}} 'InnerC0' [5:17 - 5:19]
// CHECK-EXPANDED-NEXT: |-ExtensionDeclScope {{.*}} 'S0' [8:1 - 9:1]
// CHECK-EXPANDED-NEXT:   `-ExtensionBodyScope {{.*}} 'S0' [8:14 - 9:1]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}} 'C0' [11:1 - 12:1]
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}} 'C0' [11:10 - 12:1]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}} 'E0' [14:1 - 17:1]
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}} 'E0' [14:9 - 17:1]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}} 'GenericS0' [19:1 - 20:1]
// CHECK-EXPANDED-NEXT:   `-GenericParamScope {{.*}} param 0 'T' [19:18 - 20:1]
// CHECK-EXPANDED-NEXT:     `-GenericParamScope {{.*}} param 1 'U' [19:21 - 20:1]
// CHECK-EXPANDED-NEXT:       `-NominalTypeBodyScope {{.*}} 'GenericS0' [19:24 - 20:1]
// CHECK-EXPANDED-NEXT: |-AbstractFunctionDeclScope {{.*}} 'genericFunc0(t:u:i:)' [22:1 - 23:1]
// CHECK-EXPANDED-NEXT:   `-GenericParamScope {{.*}} param 0 'T' [22:19 - 23:1]
// CHECK-EXPANDED-NEXT:     `-GenericParamScope {{.*}} param 1 'U' [22:22 - 23:1]
// CHECK-EXPANDED-NEXT:       `-AbstractFunctionParamsScope {{.*}} [22:24 - 23:1]
// CHECK-EXPANDED-NEXT:         |-DefaultArgumentInitializerScope {{.*}} [22:46 - 22:46]
// CHECK-EXPANDED-NEXT:         `-PureFunctionBodyScope {{.*}} [22:50 - 23:1]
// CHECK-EXPANDED-NEXT:           `-BraceStmtScope {{.*}} [22:50 - 23:1]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}} 'ContainsGenerics0' [25:1 - 31:1]
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}} 'ContainsGenerics0' [25:25 - 31:1]
// CHECK-EXPANDED-NEXT:     |-AbstractFunctionDeclScope {{.*}} 'init(t:u:)' [26:3 - 27:3]
// CHECK-EXPANDED-NEXT:       `-GenericParamScope {{.*}} param 0 'T' [26:8 - 27:3]
// CHECK-EXPANDED-NEXT:         `-GenericParamScope {{.*}} param 1 'U' [26:11 - 27:3]
// CHECK-EXPANDED-NEXT:           `-AbstractFunctionParamsScope {{.*}} [26:13 - 27:3]
// CHECK-EXPANDED-NEXT:             `-MethodBodyScope {{.*}} [26:26 - 27:3]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}} [26:26 - 27:3]
// CHECK-EXPANDED-NEXT:     `-AbstractFunctionDeclScope {{.*}} 'deinit' [29:3 - 30:3]
// CHECK-EXPANDED-NEXT:       `-AbstractFunctionParamsScope {{.*}} [29:3 - 30:3]
// CHECK-EXPANDED-NEXT:         `-MethodBodyScope {{.*}} [29:10 - 30:3]
// CHECK-EXPANDED-NEXT:           `-BraceStmtScope {{.*}} [29:10 - 30:3]
// CHECK-EXPANDED-NEXT: |-TypeAliasDeclScope {{.*}} <no extended nominal?!> [33:1 - 33:32]
// CHECK-EXPANDED-NEXT:   `-GenericParamScope {{.*}} param 0 'T' [33:25 - 33:32]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}} 'OtherArchStruct' [38:1 - 38:26]
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}} 'OtherArchStruct' [38:24 - 38:26]
// CHECK-EXPANDED-NEXT: |-AbstractFunctionDeclScope {{.*}} 'functionBodies1(a:b:)' [41:1 - 100:1]
// CHECK-EXPANDED-NEXT:   `-AbstractFunctionParamsScope {{.*}} [41:21 - 100:1]
// CHECK-EXPANDED-NEXT:     `-PureFunctionBodyScope {{.*}} [41:39 - 100:1]
// CHECK-EXPANDED-NEXT:       `-BraceStmtScope {{.*}} [41:39 - 100:1]
// CHECK-EXPANDED-NEXT:         `-PatternEntryDeclScope {{.*}} entry 0 'x1' 'x2' [42:7 - 99:38]
// CHECK-EXPANDED-NEXT:           |-PatternEntryInitializerScope {{.*}} entry 0 'x1' 'x2' [42:18 - 42:23]
// CHECK-EXPANDED-NEXT:           `-PatternEntryUseScope {{.*}} entry 0 'x1' 'x2' [42:23 - 99:38]
// CHECK-EXPANDED-NEXT:             `-PatternEntryDeclScope {{.*}} entry 1 'y1' 'y2' [43:7 - 99:38]
// CHECK-EXPANDED-NEXT:               |-PatternEntryInitializerScope {{.*}} entry 1 'y1' 'y2' [43:18 - 43:23]
// CHECK-EXPANDED-NEXT:               `-PatternEntryUseScope {{.*}} entry 1 'y1' 'y2' [43:23 - 99:38]
// CHECK-EXPANDED-NEXT:                 `-PatternEntryDeclScope {{.*}} entry 0 'z1' 'z2' [44:7 - 99:38]
// CHECK-EXPANDED-NEXT:                   |-PatternEntryInitializerScope {{.*}} entry 0 'z1' 'z2' [44:18 - 44:23]
// CHECK-EXPANDED-NEXT:                   `-PatternEntryUseScope {{.*}} entry 0 'z1' 'z2' [44:23 - 99:38]
// CHECK-EXPANDED-NEXT:                     |-BraceStmtScope {{.*}} [45:6 - 52:3]
// CHECK-EXPANDED-NEXT:                       `-PatternEntryDeclScope {{.*}} entry 0 'a1' [46:9 - 51:5]
// CHECK-EXPANDED-NEXT:                         |-PatternEntryInitializerScope {{.*}} entry 0 'a1' [46:14 - 46:14]
// CHECK-EXPANDED-NEXT:                         `-PatternEntryUseScope {{.*}} entry 0 'a1' [46:14 - 51:5]
// CHECK-EXPANDED-NEXT:                           `-PatternEntryDeclScope {{.*}} entry 0 'a2' [47:9 - 51:5]
// CHECK-EXPANDED-NEXT:                             |-PatternEntryInitializerScope {{.*}} entry 0 'a2' [47:14 - 47:14]
// CHECK-EXPANDED-NEXT:                             `-PatternEntryUseScope {{.*}} entry 0 'a2' [47:14 - 51:5]
// CHECK-EXPANDED-NEXT:                               `-BraceStmtScope {{.*}} [48:8 - 51:5]
// CHECK-EXPANDED-NEXT:                                 `-PatternEntryDeclScope {{.*}} entry 0 'b1' [49:11 - 50:16]
// CHECK-EXPANDED-NEXT:                                   |-PatternEntryInitializerScope {{.*}} entry 0 'b1' [49:16 - 49:16]
// CHECK-EXPANDED-NEXT:                                   `-PatternEntryUseScope {{.*}} entry 0 'b1' [49:16 - 50:16]
// CHECK-EXPANDED-NEXT:                                     `-PatternEntryDeclScope {{.*}} entry 0 'b2' [50:11 - 50:16]
// CHECK-EXPANDED-NEXT:                                       |-PatternEntryInitializerScope {{.*}} entry 0 'b2' [50:16 - 50:16]
// CHECK-EXPANDED-NEXT:                                       `-PatternEntryUseScope {{.*}} entry 0 'b2' [50:16 - 50:16]
// CHECK-EXPANDED-NEXT:                     |-BraceStmtScope {{.*}} [53:6 - 56:3]
// CHECK-EXPANDED-NEXT:                       `-PatternEntryDeclScope {{.*}} entry 0 'b1' [54:9 - 55:14]
// CHECK-EXPANDED-NEXT:                         |-PatternEntryInitializerScope {{.*}} entry 0 'b1' [54:14 - 54:14]
// CHECK-EXPANDED-NEXT:                         `-PatternEntryUseScope {{.*}} entry 0 'b1' [54:14 - 55:14]
// CHECK-EXPANDED-NEXT:                           `-PatternEntryDeclScope {{.*}} entry 0 'b2' [55:9 - 55:14]
// CHECK-EXPANDED-NEXT:                             |-PatternEntryInitializerScope {{.*}} entry 0 'b2' [55:14 - 55:14]
// CHECK-EXPANDED-NEXT:                             `-PatternEntryUseScope {{.*}} entry 0 'b2' [55:14 - 55:14]
// CHECK-EXPANDED-NEXT:                     |-AbstractFunctionDeclScope {{.*}} 'f(_:)' [57:3 - 57:38]
// CHECK-EXPANDED-NEXT:                       `-AbstractFunctionParamsScope {{.*}} [57:9 - 57:38]
// CHECK-EXPANDED-NEXT:                         `-PureFunctionBodyScope {{.*}} [57:27 - 57:38]
// CHECK-EXPANDED-NEXT:                           `-BraceStmtScope {{.*}} [57:27 - 57:38]
// CHECK-EXPANDED-NEXT:                     `-PatternEntryDeclScope {{.*}} entry 0 'f2' [58:7 - 99:38]
// CHECK-EXPANDED-NEXT:                       |-PatternEntryInitializerScope {{.*}} entry 0 'f2' [58:12 - 58:16]
// CHECK-EXPANDED-NEXT:                       `-PatternEntryUseScope {{.*}} entry 0 'f2' [58:16 - 99:38]
// CHECK-EXPANDED-NEXT:                         |-NominalTypeDeclScope {{.*}} 'S7' [59:3 - 59:15]
// CHECK-EXPANDED-NEXT:                           `-NominalTypeBodyScope {{.*}} 'S7' [59:13 - 59:15]
// CHECK-EXPANDED-NEXT:                         |-TypeAliasDeclScope {{.*}} <no extended nominal?!> [60:3 - 60:23]
// CHECK-EXPANDED-NEXT:                         |-IfStmtScope {{.*}} [62:3 - 66:3]
// CHECK-EXPANDED-NEXT:                           |-IfConditionalClauseScope {{.*}} index 0 [62:6 - 64:3]
// CHECK-EXPANDED-NEXT:                             |-StatementConditionElementPatternScope {{.*}}let b1{{.*}} [62:6 - 62:10]
// CHECK-EXPANDED-NEXT:                             `-IfConditionalClauseScope {{.*}} index 1 [62:18 - 64:3]
// CHECK-EXPANDED-NEXT:                               |-StatementConditionElementPatternScope {{.*}}let b2{{.*}} [62:18 - 62:22]
// CHECK-EXPANDED-NEXT:                               `-BraceStmtScope {{.*}} [62:29 - 64:3]
// CHECK-EXPANDED-NEXT:                                 `-PatternEntryDeclScope {{.*}} entry 0 'c1' [63:9 - 63:14]
// CHECK-EXPANDED-NEXT:                                   |-PatternEntryInitializerScope {{.*}} entry 0 'c1' [63:14 - 63:14]
// CHECK-EXPANDED-NEXT:                                   `-PatternEntryUseScope {{.*}} entry 0 'c1' [63:14 - 63:14]
// CHECK-EXPANDED-NEXT:                           `-BraceStmtScope {{.*}} [64:10 - 66:3]
// CHECK-EXPANDED-NEXT:                             `-PatternEntryDeclScope {{.*}} entry 0 'c2' [65:9 - 65:14]
// CHECK-EXPANDED-NEXT:                               |-PatternEntryInitializerScope {{.*}} entry 0 'c2' [65:14 - 65:14]
// CHECK-EXPANDED-NEXT:                               `-PatternEntryUseScope {{.*}} entry 0 'c2' [65:14 - 65:14]
// CHECK-EXPANDED-NEXT:                         `-GuardStmtScope {{.*}} [68:3 - 99:38]
// CHECK-EXPANDED-NEXT:                           |-GuardConditionalClauseScope {{.*}} index 0 [68:9 - 68:53]
// CHECK-EXPANDED-NEXT:                             |-StatementConditionElementPatternScope {{.*}}let b1{{.*}} [68:9 - 68:13]
// CHECK-EXPANDED-NEXT:                             `-GuardConditionalClauseScope {{.*}} index 1 [68:21 - 68:53]
// CHECK-EXPANDED-NEXT:                               |-WholeClosureScope {{.*}} [68:21 - 68:30]
// CHECK-EXPANDED-NEXT:                                 `-ClosureBodyScope {{.*}} [68:21 - 68:30]
// CHECK-EXPANDED-NEXT:                                   `-BraceStmtScope {{.*}} [68:21 - 68:30]
// CHECK-EXPANDED-NEXT:                               `-GuardConditionalClauseScope {{.*}} index 2 [68:37 - 68:53]
// CHECK-EXPANDED-NEXT:                                 `-StatementConditionElementPatternScope {{.*}}let b2{{.*}} [68:37 - 68:41]
// CHECK-EXPANDED-NEXT:                           |-BraceStmtScope {{.*}} [68:53 - 71:3]
// CHECK-EXPANDED-NEXT:                             `-PatternEntryDeclScope {{.*}} entry 0 'c' [69:9 - 69:13]
// CHECK-EXPANDED-NEXT:                               |-PatternEntryInitializerScope {{.*}} entry 0 'c' [69:13 - 69:13]
// CHECK-EXPANDED-NEXT:                               `-PatternEntryUseScope {{.*}} entry 0 'c' [69:13 - 69:13]
// CHECK-EXPANDED-NEXT:                           `-GuardUseScope lookup parent:  [68:37 - 68:41] [71:3 - 99:38]
// CHECK-EXPANDED-NEXT:                             |-WhileConditionalClauseScope {{.*}} index 0 [73:9 - 75:3]
// CHECK-EXPANDED-NEXT:                               |-StatementConditionElementPatternScope {{.*}}let b3{{.*}} [73:9 - 73:13]
// CHECK-EXPANDED-NEXT:                               `-WhileConditionalClauseScope {{.*}} index 1 [73:21 - 75:3]
// CHECK-EXPANDED-NEXT:                                 |-StatementConditionElementPatternScope {{.*}}let b4{{.*}} [73:21 - 73:25]
// CHECK-EXPANDED-NEXT:                                 `-BraceStmtScope {{.*}} [73:32 - 75:3]
// CHECK-EXPANDED-NEXT:                                   `-PatternEntryDeclScope {{.*}} entry 0 'c' [74:9 - 74:13]
// CHECK-EXPANDED-NEXT:                                     |-PatternEntryInitializerScope {{.*}} entry 0 'c' [74:13 - 74:13]
// CHECK-EXPANDED-NEXT:                                     `-PatternEntryUseScope {{.*}} entry 0 'c' [74:13 - 74:13]
// CHECK-EXPANDED-NEXT:                             |-RepeatWhileScope {{.*}} [77:3 - 77:20]
// CHECK-EXPANDED-NEXT:                               `-BraceStmtScope {{.*}} [77:10 - 77:12]
// CHECK-EXPANDED-NEXT:                             |-ForEachStmtScope {{.*}} [79:3 - 81:3]
// CHECK-EXPANDED-NEXT:                               `-ForEachPatternScope {{.*}} [79:52 - 81:3]
// CHECK-EXPANDED-NEXT:                                 `-BraceStmtScope {{.*}} [79:63 - 81:3]
// CHECK-EXPANDED-NEXT:                             |-DoCatchStmtScope {{.*}} [83:3 - 87:3]
// CHECK-EXPANDED-NEXT:                               |-BraceStmtScope {{.*}} [83:6 - 85:3]
// CHECK-EXPANDED-NEXT:                               |-CatchStmtScope {{.*}} [85:31 - 86:3]
// CHECK-EXPANDED-NEXT:                                 `-BraceStmtScope {{.*}} [85:54 - 86:3]
// CHECK-EXPANDED-NEXT:                               `-CatchStmtScope {{.*}} [86:11 - 87:3]
// CHECK-EXPANDED-NEXT:                                 `-BraceStmtScope {{.*}} [86:11 - 87:3]
// CHECK-EXPANDED-NEXT:                             |-SwitchStmtScope {{.*}} [89:3 - 98:3]
// CHECK-EXPANDED-NEXT:                               |-CaseStmtScope {{.*}} [90:29 - 91:10]
// CHECK-EXPANDED-NEXT:                                 `-BraceStmtScope {{.*}} [91:5 - 91:10]
// CHECK-EXPANDED-NEXT:                               |-CaseStmtScope {{.*}} [94:5 - 94:10]
// CHECK-EXPANDED-NEXT:                                 `-BraceStmtScope {{.*}} [94:5 - 94:10]
// CHECK-EXPANDED-NEXT:                               `-CaseStmtScope {{.*}} [97:5 - 97:10]
// CHECK-EXPANDED-NEXT:                                 `-BraceStmtScope {{.*}} [97:5 - 97:10]
// CHECK-EXPANDED-NEXT:                             `-ForEachStmtScope {{.*}} [99:3 - 99:38]
// CHECK-EXPANDED-NEXT:                               `-ForEachPatternScope {{.*}} [99:36 - 99:38]
// CHECK-EXPANDED-NEXT:                                 `-BraceStmtScope {{.*}} [99:36 - 99:38]
// CHECK-EXPANDED-NEXT: |-AbstractFunctionDeclScope {{.*}} 'throwing()' [102:1 - 102:26]
// CHECK-EXPANDED-NEXT:   `-AbstractFunctionParamsScope {{.*}} [102:14 - 102:26]
// CHECK-EXPANDED-NEXT:     `-PureFunctionBodyScope {{.*}} [102:24 - 102:26]
// CHECK-EXPANDED-NEXT:       `-BraceStmtScope {{.*}} [102:24 - 102:26]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}} 'MyError' [104:1 - 106:1]
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}} 'MyError' [104:24 - 106:1]
// CHECK-EXPANDED-NEXT:     `-PatternEntryDeclScope {{.*}} entry 0 'value' [105:7 - 105:14]
// CHECK-EXPANDED-NEXT:       `-PatternEntryUseScope {{.*}} entry 0 'value' [105:14 - 105:14]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}} 'MyEnum' [108:1 - 112:1]
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}} 'MyEnum' [108:13 - 112:1]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}} 'StructContainsAbstractStorageDecls' [114:1 - 130:1]
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}} 'StructContainsAbstractStorageDecls' [114:43 - 130:1]
// CHECK-EXPANDED-NEXT:     |-SubscriptDeclScope {{.*}} scope_map.(file).StructContainsAbstractStorageDecls.subscript(_:_:)@SOURCE_DIR/test/NameBinding/scope_map.swift:115:3 [115:3 - 121:3]
// CHECK-EXPANDED-NEXT:       `-AbstractFunctionParamsScope {{.*}} [115:13 - 121:3]
// CHECK-EXPANDED-NEXT:         |-AbstractFunctionDeclScope {{.*}} '_' [116:5 - 117:5]
// CHECK-EXPANDED-NEXT:           `-MethodBodyScope {{.*}} [116:9 - 117:5]
// CHECK-EXPANDED-NEXT:             `-BraceStmtScope {{.*}} [116:9 - 117:5]
// CHECK-EXPANDED-NEXT:         `-AbstractFunctionDeclScope {{.*}} '_' [118:5 - 120:5]
// CHECK-EXPANDED-NEXT:           `-MethodBodyScope {{.*}} [118:9 - 120:5]
// CHECK-EXPANDED-NEXT:             `-BraceStmtScope {{.*}} [118:9 - 120:5]
// CHECK-EXPANDED-NEXT:     `-PatternEntryDeclScope {{.*}} entry 0 'computed' [123:7 - 129:3]
// CHECK-EXPANDED-NEXT:       `-PatternEntryUseScope {{.*}} entry 0 'computed' [123:17 - 129:3]
// CHECK-EXPANDED-NEXT:         `-VarDeclScope {{.*}} scope_map.(file).StructContainsAbstractStorageDecls.computed@SOURCE_DIR/test/NameBinding/scope_map.swift:123:7 [123:21 - 129:3]
// CHECK-EXPANDED-NEXT:           |-AbstractFunctionDeclScope {{.*}} '_' [124:5 - 126:5]
// CHECK-EXPANDED-NEXT:             `-MethodBodyScope {{.*}} [124:9 - 126:5]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}} [124:9 - 126:5]
// CHECK-EXPANDED-NEXT:           `-AbstractFunctionDeclScope {{.*}} '_' [127:5 - 128:5]
// CHECK-EXPANDED-NEXT:             `-MethodBodyScope {{.*}} [127:9 - 128:5]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}} [127:9 - 128:5]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}} 'ClassWithComputedProperties' [132:1 - 140:1]
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}} 'ClassWithComputedProperties' [132:35 - 140:1]
// CHECK-EXPANDED-NEXT:     |-PatternEntryDeclScope {{.*}} entry 0 'willSetProperty' [133:7 - 135:3]
// CHECK-EXPANDED-NEXT:       |-PatternEntryInitializerScope {{.*}} entry 0 'willSetProperty' [133:30 - 133:30]
// CHECK-EXPANDED-NEXT:       `-PatternEntryUseScope {{.*}} entry 0 'willSetProperty' [133:30 - 135:3]
// CHECK-EXPANDED-NEXT:         `-VarDeclScope {{.*}} scope_map.(file).ClassWithComputedProperties.willSetProperty@SOURCE_DIR/test/NameBinding/scope_map.swift:133:7 [133:32 - 135:3]
// CHECK-EXPANDED-NEXT:           `-AbstractFunctionDeclScope {{.*}} '_' [134:5 - 134:15]
// CHECK-EXPANDED-NEXT:             `-MethodBodyScope {{.*}} [134:13 - 134:15]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}} [134:13 - 134:15]
// CHECK-EXPANDED-NEXT:     `-PatternEntryDeclScope {{.*}} entry 0 'didSetProperty' [137:7 - 139:3]
// CHECK-EXPANDED-NEXT:       |-PatternEntryInitializerScope {{.*}} entry 0 'didSetProperty' [137:29 - 137:29]
// CHECK-EXPANDED-NEXT:       `-PatternEntryUseScope {{.*}} entry 0 'didSetProperty' [137:29 - 139:3]
// CHECK-EXPANDED-NEXT:         `-VarDeclScope {{.*}} scope_map.(file).ClassWithComputedProperties.didSetProperty@SOURCE_DIR/test/NameBinding/scope_map.swift:137:7 [137:31 - 139:3]
// CHECK-EXPANDED-NEXT:           `-AbstractFunctionDeclScope {{.*}} '_' [138:5 - 138:14]
// CHECK-EXPANDED-NEXT:             `-MethodBodyScope {{.*}} [138:12 - 138:14]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}} [138:12 - 138:14]
// CHECK-EXPANDED-NEXT: |-AbstractFunctionDeclScope {{.*}} 'funcWithComputedProperties(i:)' [142:1 - 155:1]
// CHECK-EXPANDED-NEXT:   `-AbstractFunctionParamsScope {{.*}} [142:32 - 155:1]
// CHECK-EXPANDED-NEXT:     `-PureFunctionBodyScope {{.*}} [142:41 - 155:1]
// CHECK-EXPANDED-NEXT:       `-BraceStmtScope {{.*}} [142:41 - 155:1]
// CHECK-EXPANDED-NEXT:         `-PatternEntryDeclScope {{.*}} entry 0 'computed' [143:7 - 154:8]
// CHECK-EXPANDED-NEXT:           `-PatternEntryUseScope {{.*}} entry 0 'computed' [143:17 - 154:8]
// CHECK-EXPANDED-NEXT:             |-VarDeclScope {{.*}} scope_map.(file).funcWithComputedProperties(i:).computed@SOURCE_DIR/test/NameBinding/scope_map.swift:143:7 [143:21 - 149:3]
// CHECK-EXPANDED-NEXT:               |-AbstractFunctionDeclScope {{.*}} '_' [144:5 - 145:5]
// CHECK-EXPANDED-NEXT:                 `-PureFunctionBodyScope {{.*}} [144:9 - 145:5]
// CHECK-EXPANDED-NEXT:                   `-BraceStmtScope {{.*}} [144:9 - 145:5]
// CHECK-EXPANDED-NEXT:               `-AbstractFunctionDeclScope {{.*}} '_' [146:5 - 148:5]
// CHECK-EXPANDED-NEXT:                 `-PureFunctionBodyScope {{.*}} [146:9 - 148:5]
// CHECK-EXPANDED-NEXT:                   `-BraceStmtScope {{.*}} [146:9 - 148:5]
// CHECK-EXPANDED-NEXT:             `-PatternEntryDeclScope {{.*}} entry 1 'stored1' 'stored2' [149:6 - 154:8]
// CHECK-EXPANDED-NEXT:               |-PatternEntryInitializerScope {{.*}} entry 1 'stored1' 'stored2' [149:31 - 149:36]
// CHECK-EXPANDED-NEXT:               `-PatternEntryUseScope {{.*}} entry 1 'stored1' 'stored2' [149:36 - 154:8]
// CHECK-EXPANDED-NEXT:                 `-PatternEntryDeclScope {{.*}} entry 2 'alsoComputed' [150:3 - 154:8]
// CHECK-EXPANDED-NEXT:                   `-PatternEntryUseScope {{.*}} entry 2 'alsoComputed' [150:21 - 154:8]
// CHECK-EXPANDED-NEXT:                     |-VarDeclScope {{.*}} scope_map.(file).funcWithComputedProperties(i:).alsoComputed@SOURCE_DIR/test/NameBinding/scope_map.swift:150:7 [150:25 - 152:3]
// CHECK-EXPANDED-NEXT:                       `-AbstractFunctionDeclScope {{.*}} '_' [150:25 - 152:3]
// CHECK-EXPANDED-NEXT:                         `-PureFunctionBodyScope {{.*}} [150:25 - 152:3]
// CHECK-EXPANDED-NEXT:                           `-BraceStmtScope {{.*}} [150:25 - 152:3]
// CHECK-EXPANDED-NEXT:                     `-BraceStmtScope {{.*}} [154:6 - 154:8]
// CHECK-EXPANDED-NEXT: |-AbstractFunctionDeclScope {{.*}} 'closures()' [157:1 - 162:1]
// CHECK-EXPANDED-NEXT:   `-AbstractFunctionParamsScope {{.*}} [157:14 - 162:1]
// CHECK-EXPANDED-NEXT:     `-PureFunctionBodyScope {{.*}} [157:17 - 162:1]
// CHECK-EXPANDED-NEXT:       `-BraceStmtScope {{.*}} [157:17 - 162:1]
// CHECK-EXPANDED-NEXT:         |-WholeClosureScope {{.*}} [158:3 - 160:3]
// CHECK-EXPANDED-NEXT:           |-ClosureParametersScope {{.*}} [158:5 - 158:10]
// CHECK-EXPANDED-NEXT:           `-ClosureBodyScope {{.*}} [158:10 - 160:3]
// CHECK-EXPANDED-NEXT:             `-BraceStmtScope {{.*}} [158:10 - 160:3]
// CHECK-EXPANDED-NEXT:               `-WholeClosureScope {{.*}} [159:12 - 159:22]
// CHECK-EXPANDED-NEXT:                 `-ClosureBodyScope {{.*}} [159:12 - 159:22]
// CHECK-EXPANDED-NEXT:                   `-BraceStmtScope {{.*}} [159:12 - 159:22]
// CHECK-EXPANDED-NEXT:         `-WholeClosureScope {{.*}} [161:3 - 161:19]
// CHECK-EXPANDED-NEXT:           |-ClosureParametersScope {{.*}} [161:5 - 161:10]
// CHECK-EXPANDED-NEXT:           `-ClosureBodyScope {{.*}} [161:10 - 161:19]
// CHECK-EXPANDED-NEXT:             `-BraceStmtScope {{.*}} [161:10 - 161:19]
// CHECK-EXPANDED-NEXT: `-TopLevelCodeScope {{.*}} [164:1 - 194:1]
// CHECK-EXPANDED-NEXT:   `-BraceStmtScope {{.*}} [164:1 - 194:1]
// CHECK-EXPANDED-NEXT:     |-WholeClosureScope {{.*}} [164:1 - 164:14]
// CHECK-EXPANDED-NEXT:       `-ClosureBodyScope {{.*}} [164:1 - 164:14]
// CHECK-EXPANDED-NEXT:         `-BraceStmtScope {{.*}} [164:1 - 164:14]
// CHECK-EXPANDED-NEXT:     |-AbstractFunctionDeclScope {{.*}} 'defaultArguments(i:j:)' [166:1 - 175:1]
// CHECK-EXPANDED-NEXT:       `-AbstractFunctionParamsScope {{.*}} [166:22 - 175:1]
// CHECK-EXPANDED-NEXT:         |-DefaultArgumentInitializerScope {{.*}} [166:32 - 166:32]
// CHECK-EXPANDED-NEXT:         |-DefaultArgumentInitializerScope {{.*}} [167:32 - 167:48]
// CHECK-EXPANDED-NEXT:           `-WholeClosureScope {{.*}} [167:32 - 167:42]
// CHECK-EXPANDED-NEXT:             `-ClosureBodyScope {{.*}} [167:32 - 167:42]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}} [167:32 - 167:42]
// CHECK-EXPANDED-NEXT:         `-PureFunctionBodyScope {{.*}} [167:51 - 175:1]
// CHECK-EXPANDED-NEXT:           `-BraceStmtScope {{.*}} [167:51 - 175:1]
// CHECK-EXPANDED-NEXT:             |-AbstractFunctionDeclScope {{.*}} 'localWithDefaults(i:j:)' [169:3 - 171:3]
// CHECK-EXPANDED-NEXT:               `-AbstractFunctionParamsScope {{.*}} [169:25 - 171:3]
// CHECK-EXPANDED-NEXT:                 |-DefaultArgumentInitializerScope {{.*}} [169:35 - 169:35]
// CHECK-EXPANDED-NEXT:                 |-DefaultArgumentInitializerScope {{.*}} [170:35 - 170:51]
// CHECK-EXPANDED-NEXT:                   `-WholeClosureScope {{.*}} [170:35 - 170:45]
// CHECK-EXPANDED-NEXT:                     `-ClosureBodyScope {{.*}} [170:35 - 170:45]
// CHECK-EXPANDED-NEXT:                       `-BraceStmtScope {{.*}} [170:35 - 170:45]
// CHECK-EXPANDED-NEXT:                 `-PureFunctionBodyScope {{.*}} [170:54 - 171:3]
// CHECK-EXPANDED-NEXT:                   `-BraceStmtScope {{.*}} [170:54 - 171:3]
// CHECK-EXPANDED-NEXT:             `-PatternEntryDeclScope {{.*}} entry 0 'a' [173:7 - 174:11]
// CHECK-EXPANDED-NEXT:               |-PatternEntryInitializerScope {{.*}} entry 0 'a' [173:11 - 174:11]
// CHECK-EXPANDED-NEXT:                 `-WholeClosureScope {{.*}} [174:3 - 174:8]
// CHECK-EXPANDED-NEXT:                   `-ClosureBodyScope {{.*}} [174:3 - 174:8]
// CHECK-EXPANDED-NEXT:                     `-BraceStmtScope {{.*}} [174:3 - 174:8]
// CHECK-EXPANDED-NEXT:               `-PatternEntryUseScope {{.*}} entry 0 'a' [174:11 - 174:11]
// CHECK-EXPANDED-NEXT:     |-NominalTypeDeclScope {{.*}} 'PatternInitializers' [177:1 - 180:1]
// CHECK-EXPANDED-NEXT:       `-NominalTypeBodyScope {{.*}} 'PatternInitializers' [177:28 - 180:1]
// CHECK-EXPANDED-NEXT:         `-PatternEntryDeclScope {{.*}} entry 0 'a' 'b' [178:7 - 179:25]
// CHECK-EXPANDED-NEXT:           |-PatternEntryInitializerScope {{.*}} entry 0 'a' 'b' [178:16 - 178:21]
// CHECK-EXPANDED-NEXT:           `-PatternEntryUseScope {{.*}} entry 0 'a' 'b' [178:21 - 179:25]
// CHECK-EXPANDED-NEXT:             `-PatternEntryDeclScope {{.*}} entry 1 'c' 'd' [179:7 - 179:25]
// CHECK-EXPANDED-NEXT:               |-PatternEntryInitializerScope {{.*}} entry 1 'c' 'd' [179:16 - 179:25]
// CHECK-EXPANDED-NEXT:               `-PatternEntryUseScope {{.*}} entry 1 'c' 'd' [179:25 - 179:25]
// CHECK-EXPANDED-NEXT:     |-NominalTypeDeclScope {{.*}} 'ProtoWithSubscript' [182:1 - 184:1]
// CHECK-EXPANDED-NEXT:       `-GenericParamScope {{.*}} param 0 'Self : ProtoWithSubscript' [182:29 - 184:1]
// CHECK-EXPANDED-NEXT:         `-NominalTypeBodyScope {{.*}} 'ProtoWithSubscript' [182:29 - 184:1]
// CHECK-EXPANDED-NEXT:           `-SubscriptDeclScope {{.*}} scope_map.(file).ProtoWithSubscript.subscript(_:)@SOURCE_DIR/test/NameBinding/scope_map.swift:183:3 [183:3 - 183:43]
// CHECK-EXPANDED-NEXT:             `-AbstractFunctionParamsScope {{.*}} [183:12 - 183:43]
// CHECK-EXPANDED-NEXT:               |-AbstractFunctionDeclScope {{.*}} '_' [183:35 - 183:35]
// CHECK-EXPANDED-NEXT:               `-AbstractFunctionDeclScope {{.*}} '_' [183:39 - 183:39]
// CHECK-EXPANDED-NEXT:     |-AbstractFunctionDeclScope {{.*}} 'localPatternsWithSharedType()' [186:1 - 188:1]
// CHECK-EXPANDED-NEXT:       `-AbstractFunctionParamsScope {{.*}} [186:33 - 188:1]
// CHECK-EXPANDED-NEXT:         `-PureFunctionBodyScope {{.*}} [186:36 - 188:1]
// CHECK-EXPANDED-NEXT:           `-BraceStmtScope {{.*}} [186:36 - 188:1]
// CHECK-EXPANDED-NEXT:             `-PatternEntryDeclScope {{.*}} entry 0 'i' [187:7 - 187:16]
// CHECK-EXPANDED-NEXT:               `-PatternEntryUseScope {{.*}} entry 0 'i' [187:7 - 187:16]
// CHECK-EXPANDED-NEXT:                 `-PatternEntryDeclScope {{.*}} entry 1 'j' [187:10 - 187:16]
// CHECK-EXPANDED-NEXT:                   `-PatternEntryUseScope {{.*}} entry 1 'j' [187:10 - 187:16]
// CHECK-EXPANDED-NEXT:                     `-PatternEntryDeclScope {{.*}} entry 2 'k' [187:13 - 187:16]
// CHECK-EXPANDED-NEXT:                       `-PatternEntryUseScope {{.*}} entry 2 'k' [187:16 - 187:16]
// CHECK-EXPANDED-NEXT:     `-NominalTypeDeclScope {{.*}} 'LazyProperties' [190:1 - 194:1]
// CHECK-EXPANDED-NEXT:       `-NominalTypeBodyScope {{.*}} 'LazyProperties' [190:22 - 194:1]
// CHECK-EXPANDED-NEXT:         |-PatternEntryDeclScope {{.*}} entry 0 'value' [191:7 - 191:20]
// CHECK-EXPANDED-NEXT:           |-PatternEntryInitializerScope {{.*}} entry 0 'value' [191:20 - 191:20]
// CHECK-EXPANDED-NEXT:           `-PatternEntryUseScope {{.*}} entry 0 'value' [191:20 - 191:20]
// CHECK-EXPANDED-NEXT:         `-PatternEntryDeclScope {{.*}} entry 0 'prop' [193:12 - 193:29]
// CHECK-EXPANDED-NEXT:           |-PatternEntryInitializerScope {{.*}} entry 0 'prop' [193:24 - 193:29]
// CHECK-EXPANDED-NEXT:           `-PatternEntryUseScope {{.*}} entry 0 'prop' [193:29 - 193:29]




// RUN: not %target-swift-frontend -dump-scope-maps 70:8,26:20,5:18,166:32,179:18,193:26 %s 2> %t.searches
// RUN: %FileCheck -check-prefix CHECK-SEARCHES %s < %t.searches


// CHECK-SEARCHES:      ***Scope at 70:8***
// CHECK-SEARCHES-NEXT: BraceStmtScope {{.*}} [68:53 - 71:3]
// CHECK-SEARCHES-NEXT: ***Scope at 26:20***
// CHECK-SEARCHES-NEXT: AbstractFunctionParamsScope {{.*}} [26:13 - 27:3]
// CHECK-SEARCHES-NEXT: ***Scope at 5:18***
// CHECK-SEARCHES-NEXT: NominalTypeBodyScope {{.*}} 'InnerC0' [5:17 - 5:19]
// CHECK-SEARCHES-NEXT: {{.*}} Module name=scope_map
// CHECK-SEARCHES-NEXT:   {{.*}} FileUnit file="SOURCE_DIR/test/NameBinding/scope_map.swift"
// CHECK-SEARCHES-NEXT:     {{.*}} StructDecl name=S0
// CHECK-SEARCHES-NEXT:       {{.*}} ClassDecl name=InnerC0
// CHECK-SEARCHES-NEXT: ***Scope at 166:32***
// CHECK-SEARCHES-NEXT: DefaultArgumentInitializerScope {{.*}} [166:32 - 166:32]
// CHECK-SEARCHES-NEXT: {{.*}} Module name=scope_map
// CHECK-SEARCHES-NEXT:   {{.*}} FileUnit file="SOURCE_DIR/test/NameBinding/scope_map.swift"
// CHECK-SEARCHES-NEXT:     {{.*}} AbstractFunctionDecl name=defaultArguments(i:j:) : (Int, Int) -> ()
// CHECK-SEARCHES-NEXT:       {{.*}} Initializer DefaultArgument index=0
// CHECK-SEARCHES-NEXT: ***Scope at 179:18***
// CHECK-SEARCHES-NEXT: PatternEntryInitializerScope {{.*}} entry 1 'c' 'd' [179:16 - 179:25]
// CHECK-SEARCHES-NEXT: {{.*}} Module name=scope_map
// CHECK-SEARCHES-NEXT:   {{.*}} FileUnit file="SOURCE_DIR/test/NameBinding/scope_map.swift"
// CHECK-SEARCHES-NEXT:     {{.*}} StructDecl name=PatternInitializers
// CHECK-SEARCHES-NEXT:       {{.*}} Initializer PatternBinding {{.*}} #1
// CHECK-SEARCHES-NEXT: ***Scope at 193:26***
// CHECK-SEARCHES-NEXT: PatternEntryInitializerScope {{.*}} entry 0 'prop' [193:24 - 193:29]
// CHECK-SEARCHES-NEXT: {{.*}} Module name=scope_map
// CHECK-SEARCHES-NEXT:   {{.*}} FileUnit file="SOURCE_DIR/test/NameBinding/scope_map.swift"
// CHECK-SEARCHES-NEXT:     {{.*}} ClassDecl name=LazyProperties
// CHECK-SEARCHES-NEXT:       {{.*}} Initializer PatternBinding {{.*}} #0
// CHECK-SEARCHES-NEXT: Local bindings: self


// CHECK-SEARCHES-NOT:  ***Complete scope map***
