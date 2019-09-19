// REQUIRES: enable-astscope-lookup
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
// CHECK-EXPANDED-NEXT: ASTSourceFileScope {{.*}}, (uncached) [1:1 - 5{{[0-9]*}}:1] 'SOURCE_DIR{{[/]}}test{{[/]}}NameBinding{{[/]}}scope_map-astscopelookup.swift'
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [5:1 - 7:1] 'S0'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [5:11 - 7:1] 'S0'
// CHECK-EXPANDED-NEXT:     `-NominalTypeDeclScope {{.*}}, [6:3 - 6:19] 'InnerC0'
// CHECK-EXPANDED-NEXT:       `-NominalTypeBodyScope {{.*}}, [6:17 - 6:19] 'InnerC0'
// CHECK-EXPANDED-NEXT: |-ExtensionDeclScope {{.*}}, [9:1 - 10:1] 'S0'
// CHECK-EXPANDED-NEXT:   `-ExtensionBodyScope {{.*}}, [9:14 - 10:1] 'S0'
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [12:1 - 13:1] 'C0'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [12:10 - 13:1] 'C0'
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [15:1 - 18:1] 'E0'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [15:9 - 18:1] 'E0'
// CHECK-EXPANDED-NEXT:     |-EnumElementScope {{.*}}, [16:8 - 16:8]
// CHECK-EXPANDED-NEXT:     `-EnumElementScope {{.*}}, [17:8 - 17:19]
// CHECK-EXPANDED-NEXT:       `-ParameterListScope {{.*}}, [17:10 - 17:19]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [20:1 - 21:1] 'GenericS0'
// CHECK-EXPANDED-NEXT:   `-GenericParamScope {{.*}}, [20:17 - 21:1] param 0 'T'
// CHECK-EXPANDED-NEXT:     `-GenericParamScope {{.*}}, [20:17 - 21:1] param 1 'U'
// CHECK-EXPANDED-NEXT:       `-NominalTypeBodyScope {{.*}}, [20:24 - 21:1] 'GenericS0'
// CHECK-EXPANDED-NEXT: |-AbstractFunctionDeclScope {{.*}}, [23:1 - 24:1] 'genericFunc0(t:u:i:)'
// CHECK-EXPANDED-NEXT:   `-GenericParamScope {{.*}}, [23:18 - 24:1] param 0 'T'
// CHECK-EXPANDED-NEXT:     `-GenericParamScope {{.*}}, [23:18 - 24:1] param 1 'U'
// CHECK-EXPANDED-NEXT:       `-ParameterListScope {{.*}}, [23:24 - 24:1]
// CHECK-EXPANDED-NEXT:         |-DefaultArgumentInitializerScope {{.*}}, [23:46 - 23:46]
// CHECK-EXPANDED-NEXT:         `-PureFunctionBodyScope {{.*}}, [23:50 - 24:1]
// CHECK-EXPANDED-NEXT:           `-BraceStmtScope {{.*}}, [23:50 - 24:1]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [26:1 - 32:1] 'ContainsGenerics0'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [26:25 - 32:1] 'ContainsGenerics0'
// CHECK-EXPANDED-NEXT:     |-AbstractFunctionDeclScope {{.*}}, [27:3 - 28:3] 'init(t:u:)'
// CHECK-EXPANDED-NEXT:       `-GenericParamScope {{.*}}, [27:7 - 28:3] param 0 'T'
// CHECK-EXPANDED-NEXT:         `-GenericParamScope {{.*}}, [27:7 - 28:3] param 1 'U'
// CHECK-EXPANDED-NEXT:           `-ParameterListScope {{.*}}, [27:13 - 28:3]
// CHECK-EXPANDED-NEXT:             `-MethodBodyScope {{.*}}, [27:26 - 28:3]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}}, [27:26 - 28:3]
// CHECK-EXPANDED-NEXT:     `-AbstractFunctionDeclScope {{.*}}, [30:3 - 31:3] 'deinit'
// CHECK-EXPANDED-NEXT:       `-ParameterListScope {{.*}}, [30:3 - 31:3]
// CHECK-EXPANDED-NEXT:         `-MethodBodyScope {{.*}}, [30:10 - 31:3]
// CHECK-EXPANDED-NEXT:           `-BraceStmtScope {{.*}}, [30:10 - 31:3]
// CHECK-EXPANDED-NEXT: |-TypeAliasDeclScope {{.*}}, [34:1 - 34:32] <no extended nominal?!>
// CHECK-EXPANDED-NEXT:   `-GenericParamScope {{.*}}, [34:24 - 34:32] param 0 'T'
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [39:1 - 39:26] 'OtherArchStruct'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [39:24 - 39:26] 'OtherArchStruct'
// CHECK-EXPANDED-NEXT: |-AbstractFunctionDeclScope {{.*}}, [42:1 - 101:1] 'functionBodies1(a:b:)'
// CHECK-EXPANDED-NEXT:   `-ParameterListScope {{.*}}, [42:21 - 101:1]
// CHECK-EXPANDED-NEXT:     `-PureFunctionBodyScope {{.*}}, [42:39 - 101:1]
// CHECK-EXPANDED-NEXT:       `-BraceStmtScope {{.*}}, [42:39 - 101:1]
// CHECK-EXPANDED-NEXT:         |-PatternEntryDeclScope {{.*}}, [43:7 - 43:23] entry 0 'x1' 'x2'
// CHECK-EXPANDED-NEXT:           `-PatternEntryInitializerScope {{.*}}, [43:18 - 43:23] entry 0 'x1' 'x2'
// CHECK-EXPANDED-NEXT:         |-PatternEntryDeclScope {{.*}}, [44:7 - 44:23] entry 1 'y1' 'y2'
// CHECK-EXPANDED-NEXT:           `-PatternEntryInitializerScope {{.*}}, [44:18 - 44:23] entry 1 'y1' 'y2'
// CHECK-EXPANDED-NEXT:         |-PatternEntryDeclScope {{.*}}, [45:7 - 45:23] entry 0 'z1' 'z2'
// CHECK-EXPANDED-NEXT:           `-PatternEntryInitializerScope {{.*}}, [45:18 - 45:23] entry 0 'z1' 'z2'
// CHECK-EXPANDED-NEXT:         |-BraceStmtScope {{.*}}, [46:6 - 53:3]
// CHECK-EXPANDED-NEXT:           |-PatternEntryDeclScope {{.*}}, [47:9 - 47:14] entry 0 'a1'
// CHECK-EXPANDED-NEXT:             `-PatternEntryInitializerScope {{.*}}, [47:14 - 47:14] entry 0 'a1'
// CHECK-EXPANDED-NEXT:           |-PatternEntryDeclScope {{.*}}, [48:9 - 48:14] entry 0 'a2'
// CHECK-EXPANDED-NEXT:             `-PatternEntryInitializerScope {{.*}}, [48:14 - 48:14] entry 0 'a2'
// CHECK-EXPANDED-NEXT:           `-BraceStmtScope {{.*}}, [49:8 - 52:5]
// CHECK-EXPANDED-NEXT:             |-PatternEntryDeclScope {{.*}}, [50:11 - 50:16] entry 0 'b1'
// CHECK-EXPANDED-NEXT:               `-PatternEntryInitializerScope {{.*}}, [50:16 - 50:16] entry 0 'b1'
// CHECK-EXPANDED-NEXT:             `-PatternEntryDeclScope {{.*}}, [51:11 - 51:16] entry 0 'b2'
// CHECK-EXPANDED-NEXT:               `-PatternEntryInitializerScope {{.*}}, [51:16 - 51:16] entry 0 'b2'
// CHECK-EXPANDED-NEXT:         |-BraceStmtScope {{.*}}, [54:6 - 57:3]
// CHECK-EXPANDED-NEXT:           |-PatternEntryDeclScope {{.*}}, [55:9 - 55:14] entry 0 'b1'
// CHECK-EXPANDED-NEXT:             `-PatternEntryInitializerScope {{.*}}, [55:14 - 55:14] entry 0 'b1'
// CHECK-EXPANDED-NEXT:           `-PatternEntryDeclScope {{.*}}, [56:9 - 56:14] entry 0 'b2'
// CHECK-EXPANDED-NEXT:             `-PatternEntryInitializerScope {{.*}}, [56:14 - 56:14] entry 0 'b2'
// CHECK-EXPANDED-NEXT:         |-AbstractFunctionDeclScope {{.*}}, [58:3 - 58:38] 'f(_:)'
// CHECK-EXPANDED-NEXT:           `-ParameterListScope {{.*}}, [58:9 - 58:38]
// CHECK-EXPANDED-NEXT:             `-PureFunctionBodyScope {{.*}}, [58:27 - 58:38]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}}, [58:27 - 58:38]
// CHECK-EXPANDED-NEXT:         |-PatternEntryDeclScope {{.*}}, [59:7 - 59:16] entry 0 'f2'
// CHECK-EXPANDED-NEXT:           `-PatternEntryInitializerScope {{.*}}, [59:12 - 59:16] entry 0 'f2'
// CHECK-EXPANDED-NEXT:         |-NominalTypeDeclScope {{.*}}, [60:3 - 60:15] 'S7'
// CHECK-EXPANDED-NEXT:           `-NominalTypeBodyScope {{.*}}, [60:13 - 60:15] 'S7'
// CHECK-EXPANDED-NEXT:         |-TypeAliasDeclScope {{.*}}, [61:3 - 61:23] <no extended nominal?!>
// CHECK-EXPANDED-NEXT:         |-IfStmtScope {{.*}}, [63:3 - 67:3]
// CHECK-EXPANDED-NEXT:           |-ConditionalClauseScope, [63:6 - 65:3] index 0
// CHECK-EXPANDED-NEXT:             `-ConditionalClausePatternUseScope, [63:18 - 65:3] let b1
// CHECK-EXPANDED-NEXT:               `-ConditionalClauseScope, [63:18 - 65:3] index 1
// CHECK-EXPANDED-NEXT:                 `-ConditionalClausePatternUseScope, [63:29 - 65:3] let b2
// CHECK-EXPANDED-NEXT:                   `-BraceStmtScope {{.*}}, [63:29 - 65:3]
// CHECK-EXPANDED-NEXT:                     `-PatternEntryDeclScope {{.*}}, [64:9 - 64:14] entry 0 'c1'
// CHECK-EXPANDED-NEXT:                       `-PatternEntryInitializerScope {{.*}}, [64:14 - 64:14] entry 0 'c1'
// CHECK-EXPANDED-NEXT:           `-BraceStmtScope {{.*}}, [65:10 - 67:3]
// CHECK-EXPANDED-NEXT:             `-PatternEntryDeclScope {{.*}}, [66:9 - 66:14] entry 0 'c2'
// CHECK-EXPANDED-NEXT:               `-PatternEntryInitializerScope {{.*}}, [66:14 - 66:14] entry 0 'c2'
// CHECK-EXPANDED-NEXT:         `-GuardStmtScope {{.*}}, [69:3 - 100:38]
// CHECK-EXPANDED-NEXT:           |-ConditionalClauseScope, [69:9 - 69:53] index 0
// CHECK-EXPANDED-NEXT:             `-ConditionalClausePatternUseScope, [69:21 - 69:53] let b1
// CHECK-EXPANDED-NEXT:               `-ConditionalClauseScope, [69:21 - 69:53] index 1
// CHECK-EXPANDED-NEXT:                 |-WholeClosureScope {{.*}}, [69:21 - 69:30]
// CHECK-EXPANDED-NEXT:                   `-ClosureBodyScope {{.*}}, [69:21 - 69:30]
// CHECK-EXPANDED-NEXT:                     `-BraceStmtScope {{.*}}, [69:21 - 69:30]
// CHECK-EXPANDED-NEXT:                 `-ConditionalClauseScope, [69:37 - 69:53] index 2
// CHECK-EXPANDED-NEXT:                   `-ConditionalClausePatternUseScope, [69:53 - 69:53] let b2
// CHECK-EXPANDED-NEXT:           |-BraceStmtScope {{.*}}, [69:53 - 72:3]
// CHECK-EXPANDED-NEXT:             `-PatternEntryDeclScope {{.*}}, [70:9 - 70:13] entry 0 'c'
// CHECK-EXPANDED-NEXT:               `-PatternEntryInitializerScope {{.*}}, [70:13 - 70:13] entry 0 'c'
// CHECK-EXPANDED-NEXT:           `-LookupParentDiversionScope, [72:3 - 100:38]
// CHECK-EXPANDED-NEXT:             |-WhileStmtScope {{.*}}, [74:3 - 76:3]
// CHECK-EXPANDED-NEXT:               `-ConditionalClauseScope, [74:9 - 76:3] index 0
// CHECK-EXPANDED-NEXT:                 `-ConditionalClausePatternUseScope, [74:21 - 76:3] let b3
// CHECK-EXPANDED-NEXT:                   `-ConditionalClauseScope, [74:21 - 76:3] index 1
// CHECK-EXPANDED-NEXT:                     `-ConditionalClausePatternUseScope, [74:32 - 76:3] let b4
// CHECK-EXPANDED-NEXT:                       `-BraceStmtScope {{.*}}, [74:32 - 76:3]
// CHECK-EXPANDED-NEXT:                         `-PatternEntryDeclScope {{.*}}, [75:9 - 75:13] entry 0 'c'
// CHECK-EXPANDED-NEXT:                           `-PatternEntryInitializerScope {{.*}}, [75:13 - 75:13] entry 0 'c'
// CHECK-EXPANDED-NEXT:             |-RepeatWhileScope {{.*}}, [78:3 - 78:20]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}}, [78:10 - 78:12]
// CHECK-EXPANDED-NEXT:             |-ForEachStmtScope {{.*}}, [80:3 - 82:3]
// CHECK-EXPANDED-NEXT:               `-ForEachPatternScope, [80:52 - 82:3]
// CHECK-EXPANDED-NEXT:                 `-BraceStmtScope {{.*}}, [80:63 - 82:3]
// CHECK-EXPANDED-NEXT:             |-DoCatchStmtScope {{.*}}, [84:3 - 88:3]
// CHECK-EXPANDED-NEXT:               |-BraceStmtScope {{.*}}, [84:6 - 86:3]
// CHECK-EXPANDED-NEXT:               |-CatchStmtScope {{.*}}, [86:31 - 87:3]
// CHECK-EXPANDED-NEXT:                 `-BraceStmtScope {{.*}}, [86:54 - 87:3]
// CHECK-EXPANDED-NEXT:               `-CatchStmtScope {{.*}}, [87:11 - 88:3]
// CHECK-EXPANDED-NEXT:                 `-BraceStmtScope {{.*}}, [87:11 - 88:3]
// CHECK-EXPANDED-NEXT:             |-SwitchStmtScope {{.*}}, [90:3 - 99:3]
// CHECK-EXPANDED-NEXT:               |-CaseStmtScope {{.*}}, [91:29 - 92:10]
// CHECK-EXPANDED-NEXT:                 `-BraceStmtScope {{.*}}, [92:5 - 92:10]
// CHECK-EXPANDED-NEXT:               |-CaseStmtScope {{.*}}, [95:5 - 95:10]
// CHECK-EXPANDED-NEXT:                 `-BraceStmtScope {{.*}}, [95:5 - 95:10]
// CHECK-EXPANDED-NEXT:               `-CaseStmtScope {{.*}}, [98:5 - 98:10]
// CHECK-EXPANDED-NEXT:                 `-BraceStmtScope {{.*}}, [98:5 - 98:10]
// CHECK-EXPANDED-NEXT:             `-ForEachStmtScope {{.*}}, [100:3 - 100:38]
// CHECK-EXPANDED-NEXT:               `-ForEachPatternScope, [100:36 - 100:38]
// CHECK-EXPANDED-NEXT:                 `-BraceStmtScope {{.*}}, [100:36 - 100:38]
// CHECK-EXPANDED-NEXT: |-AbstractFunctionDeclScope {{.*}}, [103:1 - 103:26] 'throwing()'
// CHECK-EXPANDED-NEXT:   `-ParameterListScope {{.*}}, [103:14 - 103:26]
// CHECK-EXPANDED-NEXT:     `-PureFunctionBodyScope {{.*}}, [103:24 - 103:26]
// CHECK-EXPANDED-NEXT:       `-BraceStmtScope {{.*}}, [103:24 - 103:26]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [105:1 - 107:1] 'MyError'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [105:24 - 107:1] 'MyError'
// CHECK-EXPANDED-NEXT:     `-PatternEntryDeclScope {{.*}}, [106:7 - 106:14] entry 0 'value'
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [109:1 - 113:1] 'MyEnum'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [109:13 - 113:1] 'MyEnum'
// CHECK-EXPANDED-NEXT:     |-EnumElementScope {{.*}}, [110:8 - 110:8]
// CHECK-EXPANDED-NEXT:     |-EnumElementScope {{.*}}, [111:8 - 111:18]
// CHECK-EXPANDED-NEXT:       `-ParameterListScope {{.*}}, [111:14 - 111:18]
// CHECK-EXPANDED-NEXT:     `-EnumElementScope {{.*}}, [112:8 - 112:8]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [115:1 - 131:1] 'StructContainsAbstractStorageDecls'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [115:43 - 131:1] 'StructContainsAbstractStorageDecls'
// CHECK-EXPANDED-NEXT:     |-SubscriptDeclScope {{.*}}, [116:3 - 122:3] main.(file).StructContainsAbstractStorageDecls.subscript(_:_:)@SOURCE_DIR/test/NameBinding/scope_map-astscopelookup.swift:116:3
// CHECK-EXPANDED-NEXT:       `-ParameterListScope {{.*}}, [116:13 - 122:3]
// CHECK-EXPANDED-NEXT:         |-AbstractFunctionDeclScope {{.*}}, [117:5 - 118:5] '_'
// CHECK-EXPANDED-NEXT:           `-MethodBodyScope {{.*}}, [117:9 - 118:5]
// CHECK-EXPANDED-NEXT:             `-BraceStmtScope {{.*}}, [117:9 - 118:5]
// CHECK-EXPANDED-NEXT:         `-AbstractFunctionDeclScope {{.*}}, [119:5 - 121:5] '_'
// CHECK-EXPANDED-NEXT:           `-MethodBodyScope {{.*}}, [119:9 - 121:5]
// CHECK-EXPANDED-NEXT:             `-BraceStmtScope {{.*}}, [119:9 - 121:5]
// CHECK-EXPANDED-NEXT:     `-PatternEntryDeclScope {{.*}}, [124:21 - 130:3] entry 0 'computed'
// CHECK-EXPANDED-NEXT:       `-VarDeclScope {{.*}}, [124:21 - 130:3] main.(file).StructContainsAbstractStorageDecls.computed@SOURCE_DIR/test/NameBinding/scope_map-astscopelookup.swift:124:7
// CHECK-EXPANDED-NEXT:         |-AbstractFunctionDeclScope {{.*}}, [125:5 - 127:5] '_'
// CHECK-EXPANDED-NEXT:           `-MethodBodyScope {{.*}}, [125:9 - 127:5]
// CHECK-EXPANDED-NEXT:             `-BraceStmtScope {{.*}}, [125:9 - 127:5]
// CHECK-EXPANDED-NEXT:         `-AbstractFunctionDeclScope {{.*}}, [128:5 - 129:5] '_'
// CHECK-EXPANDED-NEXT:           `-MethodBodyScope {{.*}}, [128:9 - 129:5]
// CHECK-EXPANDED-NEXT:             `-BraceStmtScope {{.*}}, [128:9 - 129:5]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [133:1 - 141:1] 'ClassWithComputedProperties'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [133:35 - 141:1] 'ClassWithComputedProperties'
// CHECK-EXPANDED-NEXT:     |-PatternEntryDeclScope {{.*}}, [134:7 - 136:3] entry 0 'willSetProperty'
// CHECK-EXPANDED-NEXT:       |-PatternEntryInitializerScope {{.*}}, [134:30 - 134:30] entry 0 'willSetProperty'
// CHECK-EXPANDED-NEXT:       `-VarDeclScope {{.*}}, [134:32 - 136:3] main.(file).ClassWithComputedProperties.willSetProperty@SOURCE_DIR/test/NameBinding/scope_map-astscopelookup.swift:134:7
// CHECK-EXPANDED-NEXT:         `-AbstractFunctionDeclScope {{.*}}, [135:5 - 135:15] '_'
// CHECK-EXPANDED-NEXT:           `-MethodBodyScope {{.*}}, [135:13 - 135:15]
// CHECK-EXPANDED-NEXT:             `-BraceStmtScope {{.*}}, [135:13 - 135:15]
// CHECK-EXPANDED-NEXT:     `-PatternEntryDeclScope {{.*}}, [138:7 - 140:3] entry 0 'didSetProperty'
// CHECK-EXPANDED-NEXT:       |-PatternEntryInitializerScope {{.*}}, [138:29 - 138:29] entry 0 'didSetProperty'
// CHECK-EXPANDED-NEXT:       `-VarDeclScope {{.*}}, [138:31 - 140:3] main.(file).ClassWithComputedProperties.didSetProperty@SOURCE_DIR/test/NameBinding/scope_map-astscopelookup.swift:138:7
// CHECK-EXPANDED-NEXT:         `-AbstractFunctionDeclScope {{.*}}, [139:5 - 139:14] '_'
// CHECK-EXPANDED-NEXT:           `-MethodBodyScope {{.*}}, [139:12 - 139:14]
// CHECK-EXPANDED-NEXT:             `-BraceStmtScope {{.*}}, [139:12 - 139:14]
// CHECK-EXPANDED-NEXT: |-AbstractFunctionDeclScope {{.*}}, [143:1 - 156:1] 'funcWithComputedProperties(i:)'
// CHECK-EXPANDED-NEXT:   `-ParameterListScope {{.*}}, [143:32 - 156:1]
// CHECK-EXPANDED-NEXT:     `-PureFunctionBodyScope {{.*}}, [143:41 - 156:1]
// CHECK-EXPANDED-NEXT:       `-BraceStmtScope {{.*}}, [143:41 - 156:1]
// CHECK-EXPANDED-NEXT:         |-PatternEntryDeclScope {{.*}}, [144:21 - 150:3] entry 0 'computed'
// CHECK-EXPANDED-NEXT:           `-VarDeclScope {{.*}}, [144:21 - 150:3] main.(file).funcWithComputedProperties(i:).computed@SOURCE_DIR/test/NameBinding/scope_map-astscopelookup.swift:144:7
// CHECK-EXPANDED-NEXT:             |-AbstractFunctionDeclScope {{.*}}, [145:5 - 146:5] '_'
// CHECK-EXPANDED-NEXT:               `-PureFunctionBodyScope {{.*}}, [145:9 - 146:5]
// CHECK-EXPANDED-NEXT:                 `-BraceStmtScope {{.*}}, [145:9 - 146:5]
// CHECK-EXPANDED-NEXT:             `-AbstractFunctionDeclScope {{.*}}, [147:5 - 149:5] '_'
// CHECK-EXPANDED-NEXT:               `-PureFunctionBodyScope {{.*}}, [147:9 - 149:5]
// CHECK-EXPANDED-NEXT:                 `-BraceStmtScope {{.*}}, [147:9 - 149:5]
// CHECK-EXPANDED-NEXT:         |-PatternEntryDeclScope {{.*}}, [150:6 - 150:36] entry 1 'stored1' 'stored2'
// CHECK-EXPANDED-NEXT:           `-PatternEntryInitializerScope {{.*}}, [150:31 - 150:36] entry 1 'stored1' 'stored2'
// CHECK-EXPANDED-NEXT:         |-PatternEntryDeclScope {{.*}}, [151:25 - 153:3] entry 2 'alsoComputed'
// CHECK-EXPANDED-NEXT:           `-VarDeclScope {{.*}}, [151:25 - 153:3] main.(file).funcWithComputedProperties(i:).alsoComputed@SOURCE_DIR/test/NameBinding/scope_map-astscopelookup.swift:151:7
// CHECK-EXPANDED-NEXT:             `-AbstractFunctionDeclScope {{.*}}, [151:25 - 153:3] '_'
// CHECK-EXPANDED-NEXT:               `-PureFunctionBodyScope {{.*}}, [151:25 - 153:3]
// CHECK-EXPANDED-NEXT:                 `-BraceStmtScope {{.*}}, [151:25 - 153:3]
// CHECK-EXPANDED-NEXT:         `-BraceStmtScope {{.*}}, [155:6 - 155:8]
// CHECK-EXPANDED-NEXT: |-AbstractFunctionDeclScope {{.*}}, [158:1 - 163:1] 'closures()'
// CHECK-EXPANDED-NEXT:   `-ParameterListScope {{.*}}, [158:14 - 163:1]
// CHECK-EXPANDED-NEXT:     `-PureFunctionBodyScope {{.*}}, [158:17 - 163:1]
// CHECK-EXPANDED-NEXT:       `-BraceStmtScope {{.*}}, [158:17 - 163:1]
// CHECK-EXPANDED-NEXT:         |-WholeClosureScope {{.*}}, [159:3 - 161:3]
// CHECK-EXPANDED-NEXT:           `-ClosureParametersScope {{.*}}, [159:5 - 161:3]
// CHECK-EXPANDED-NEXT:             `-ClosureBodyScope {{.*}}, [159:10 - 161:3]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}}, [159:10 - 161:3]
// CHECK-EXPANDED-NEXT:                 `-WholeClosureScope {{.*}}, [160:12 - 160:22]
// CHECK-EXPANDED-NEXT:                   `-ClosureBodyScope {{.*}}, [160:12 - 160:22]
// CHECK-EXPANDED-NEXT:                     `-BraceStmtScope {{.*}}, [160:12 - 160:22]
// CHECK-EXPANDED-NEXT:         `-WholeClosureScope {{.*}}, [162:3 - 162:19]
// CHECK-EXPANDED-NEXT:           `-ClosureParametersScope {{.*}}, [162:5 - 162:19]
// CHECK-EXPANDED-NEXT:             `-ClosureBodyScope {{.*}}, [162:10 - 162:19]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}}, [162:10 - 162:19]
// CHECK-EXPANDED-NEXT: `-TopLevelCodeScope {{.*}}, [165:1 - 195:1]
// CHECK-EXPANDED-NEXT:   `-BraceStmtScope {{.*}}, [165:1 - 195:1]
// CHECK-EXPANDED-NEXT:     |-WholeClosureScope {{.*}}, [165:1 - 165:14]
// CHECK-EXPANDED-NEXT:       `-ClosureBodyScope {{.*}}, [165:1 - 165:14]
// CHECK-EXPANDED-NEXT:         `-BraceStmtScope {{.*}}, [165:1 - 165:14]
// CHECK-EXPANDED-NEXT:     |-AbstractFunctionDeclScope {{.*}}, [167:1 - 176:1] 'defaultArguments(i:j:)'
// CHECK-EXPANDED-NEXT:       `-ParameterListScope {{.*}}, [167:22 - 176:1]
// CHECK-EXPANDED-NEXT:         |-DefaultArgumentInitializerScope {{.*}}, [167:32 - 167:32]
// CHECK-EXPANDED-NEXT:         |-DefaultArgumentInitializerScope {{.*}}, [168:32 - 168:48]
// CHECK-EXPANDED-NEXT:           `-WholeClosureScope {{.*}}, [168:32 - 168:42]
// CHECK-EXPANDED-NEXT:             `-ClosureBodyScope {{.*}}, [168:32 - 168:42]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}}, [168:32 - 168:42]
// CHECK-EXPANDED-NEXT:         `-PureFunctionBodyScope {{.*}}, [168:51 - 176:1]
// CHECK-EXPANDED-NEXT:           `-BraceStmtScope {{.*}}, [168:51 - 176:1]
// CHECK-EXPANDED-NEXT:             |-AbstractFunctionDeclScope {{.*}}, [170:3 - 172:3] 'localWithDefaults(i:j:)'
// CHECK-EXPANDED-NEXT:               `-ParameterListScope {{.*}}, [170:25 - 172:3]
// CHECK-EXPANDED-NEXT:                 |-DefaultArgumentInitializerScope {{.*}}, [170:35 - 170:35]
// CHECK-EXPANDED-NEXT:                 |-DefaultArgumentInitializerScope {{.*}}, [171:35 - 171:51]
// CHECK-EXPANDED-NEXT:                   `-WholeClosureScope {{.*}}, [171:35 - 171:45]
// CHECK-EXPANDED-NEXT:                     `-ClosureBodyScope {{.*}}, [171:35 - 171:45]
// CHECK-EXPANDED-NEXT:                       `-BraceStmtScope {{.*}}, [171:35 - 171:45]
// CHECK-EXPANDED-NEXT:                 `-PureFunctionBodyScope {{.*}}, [171:54 - 172:3]
// CHECK-EXPANDED-NEXT:                   `-BraceStmtScope {{.*}}, [171:54 - 172:3]
// CHECK-EXPANDED-NEXT:             `-PatternEntryDeclScope {{.*}}, [174:7 - 175:11] entry 0 'a'
// CHECK-EXPANDED-NEXT:               `-PatternEntryInitializerScope {{.*}}, [174:11 - 175:11] entry 0 'a'
// CHECK-EXPANDED-NEXT:                 `-WholeClosureScope {{.*}}, [175:3 - 175:8]
// CHECK-EXPANDED-NEXT:                   `-ClosureBodyScope {{.*}}, [175:3 - 175:8]
// CHECK-EXPANDED-NEXT:                     `-BraceStmtScope {{.*}}, [175:3 - 175:8]
// CHECK-EXPANDED-NEXT:     |-NominalTypeDeclScope {{.*}}, [178:1 - 181:1] 'PatternInitializers'
// CHECK-EXPANDED-NEXT:       `-NominalTypeBodyScope {{.*}}, [178:28 - 181:1] 'PatternInitializers'
// CHECK-EXPANDED-NEXT:         |-PatternEntryDeclScope {{.*}}, [179:7 - 179:21] entry 0 'a' 'b'
// CHECK-EXPANDED-NEXT:           `-PatternEntryInitializerScope {{.*}}, [179:16 - 179:21] entry 0 'a' 'b'
// CHECK-EXPANDED-NEXT:         `-PatternEntryDeclScope {{.*}}, [180:7 - 180:25] entry 1 'c' 'd'
// CHECK-EXPANDED-NEXT:           `-PatternEntryInitializerScope {{.*}}, [180:16 - 180:25] entry 1 'c' 'd'
// CHECK-EXPANDED-NEXT:     |-NominalTypeDeclScope {{.*}}, [183:1 - 185:1] 'ProtoWithSubscript'
// CHECK-EXPANDED-NEXT:       `-GenericParamScope {{.*}}, [183:29 - 185:1] param 0 'Self : ProtoWithSubscript'
// CHECK-EXPANDED-NEXT:         `-NominalTypeBodyScope {{.*}}, [183:29 - 185:1] 'ProtoWithSubscript'
// CHECK-EXPANDED-NEXT:           `-SubscriptDeclScope {{.*}}, [184:3 - 184:43] main.(file).ProtoWithSubscript.subscript(_:)@SOURCE_DIR/test/NameBinding/scope_map-astscopelookup.swift:184:3
// CHECK-EXPANDED-NEXT:             `-ParameterListScope {{.*}}, [184:12 - 184:43]
// CHECK-EXPANDED-NEXT:               |-AbstractFunctionDeclScope {{.*}}, [184:35 - 184:35] '_'
// CHECK-EXPANDED-NEXT:               `-AbstractFunctionDeclScope {{.*}}, [184:39 - 184:39] '_'
// CHECK-EXPANDED-NEXT:     |-AbstractFunctionDeclScope {{.*}}, [187:1 - 189:1] 'localPatternsWithSharedType()'
// CHECK-EXPANDED-NEXT:       `-ParameterListScope {{.*}}, [187:33 - 189:1]
// CHECK-EXPANDED-NEXT:         `-PureFunctionBodyScope {{.*}}, [187:36 - 189:1]
// CHECK-EXPANDED-NEXT:           `-BraceStmtScope {{.*}}, [187:36 - 189:1]
// CHECK-EXPANDED-NEXT:             |-PatternEntryDeclScope {{.*}}, [188:7 - 188:7] entry 0 'i'
// CHECK-EXPANDED-NEXT:             |-PatternEntryDeclScope {{.*}}, [188:10 - 188:10] entry 1 'j'
// CHECK-EXPANDED-NEXT:             `-PatternEntryDeclScope {{.*}}, [188:13 - 188:16] entry 2 'k'
// CHECK-EXPANDED-NEXT:     `-NominalTypeDeclScope {{.*}}, [191:1 - 195:1] 'LazyProperties'
// CHECK-EXPANDED-NEXT:       `-NominalTypeBodyScope {{.*}}, [191:22 - 195:1] 'LazyProperties'
// CHECK-EXPANDED-NEXT:         |-PatternEntryDeclScope {{.*}}, [192:7 - 192:20] entry 0 'value'
// CHECK-EXPANDED-NEXT:           `-PatternEntryInitializerScope {{.*}}, [192:20 - 192:20] entry 0 'value'
// CHECK-EXPANDED-NEXT:         `-PatternEntryDeclScope {{.*}}, [194:12 - 194:29] entry 0 'prop'
// CHECK-EXPANDED-NEXT:           `-PatternEntryInitializerScope {{.*}}, [194:24 - 194:29] entry 0 'prop'



// RUN: not %target-swift-frontend -dump-scope-maps 71:8,27:20,6:18,167:32,180:18,194:26 %s 2> %t.searches
// RUN: %FileCheck -check-prefix CHECK-SEARCHES %s < %t.searches

// CHECK-SEARCHES:      ***Scope at 71:8***
// CHECK-SEARCHES-NEXT: BraceStmtScope {{.*}}, [69:53 - 7{{[0-9]}}:3]
// CHECK-SEARCHES-NEXT: Local bindings: c
// CHECK-SEARCHES-NEXT: ***Scope at 27:20***
// CHECK-SEARCHES-NEXT: ParameterListScope {{.*}}, [27:13 - 28:3]
// CHECK-SEARCHES-NEXT: ***Scope at 6:18***
// CHECK-SEARCHES-NEXT: NominalTypeBodyScope {{.*}}, [6:17 - 6:19] 'InnerC0'
// CHECK-SEARCHES-NEXT: {{.*}} Module name=main
// CHECK-SEARCHES-NEXT:   {{.*}} FileUnit file="SOURCE_DIR/test/NameBinding/scope_map-astscopelookup.swift"
// CHECK-SEARCHES-NEXT:     {{.*}} StructDecl name=S0
// CHECK-SEARCHES-NEXT:       {{.*}} ClassDecl name=InnerC0
// CHECK-SEARCHES-NEXT: ***Scope at 167:32***
// CHECK-SEARCHES-NEXT: DefaultArgumentInitializerScope {{.*}}, [167:32 - 167:32]
// CHECK-SEARCHES-NEXT: {{.*}} Module name=main
// CHECK-SEARCHES-NEXT:   {{.*}} FileUnit file="SOURCE_DIR/test/NameBinding/scope_map-astscopelookup.swift"
// CHECK-SEARCHES-NEXT:     {{.*}} AbstractFunctionDecl name=defaultArguments(i:j:) : (Int, Int) -> ()
// CHECK-SEARCHES-NEXT:       {{.*}} Initializer DefaultArgument index=0
// CHECK-SEARCHES-NEXT: ***Scope at 180:18***
// CHECK-SEARCHES-NEXT: PatternEntryInitializerScope {{.*}}, [180:16 - 180:25] entry 1 'c' 'd'
// CHECK-SEARCHES-NEXT: {{.*}} Module name=main
// CHECK-SEARCHES-NEXT:   {{.*}} FileUnit file="SOURCE_DIR/test/NameBinding/scope_map-astscopelookup.swift"
// CHECK-SEARCHES-NEXT:     {{.*}} StructDecl name=PatternInitializers
// CHECK-SEARCHES-NEXT:       {{.*}} Initializer PatternBinding {{.*}} #1
// CHECK-SEARCHES-NEXT: ***Scope at 194:26***
// CHECK-SEARCHES-NEXT: PatternEntryInitializerScope {{.*}}, [194:24 - 194:29] entry 0 'prop'
// CHECK-SEARCHES-NEXT: {{.*}} Module name=main
// CHECK-SEARCHES-NEXT:   {{.*}} FileUnit file="SOURCE_DIR/test/NameBinding/scope_map-astscopelookup.swift"
// CHECK-SEARCHES-NEXT:     {{.*}} ClassDecl name=LazyProperties
// CHECK-SEARCHES-NEXT:       {{.*}} Initializer PatternBinding {{.*}} #0
// CHECK-SEARCHES-NEXT: Local bindings: self


// CHECK-SEARCHES-NOT:  ***Complete scope map***

// REQUIRES: asserts
// absence of assertions can change the "uncached" bit and cause failures
