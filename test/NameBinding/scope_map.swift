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
// CHECK-EXPANDED-NEXT: ASTSourceFileScope {{.*}} [SOURCE_DIR/test/NameBinding/scope_map.swift:1:1 - line:{{.*}}:1] 'SOURCE_DIR/test/NameBinding/scope_map.swift'
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:4:1 - line:6:1]   'S0'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:4:11 - line:6:1]   'S0'
// CHECK-EXPANDED-NEXT:     `-NominalTypeDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:5:3 - line:5:19]   'InnerC0'
// CHECK-EXPANDED-NEXT:       `-NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:5:17 - line:5:19]   'InnerC0'
// CHECK-EXPANDED-NEXT: |-ExtensionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:8:1 - line:9:1]   'S0'
// CHECK-EXPANDED-NEXT:   `-ExtensionBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:8:14 - line:9:1]   'S0'
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:11:1 - line:12:1]   'C0'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:11:10 - line:12:1]   'C0'
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:14:1 - line:17:1]   'E0'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:14:9 - line:17:1]   'E0'
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:19:1 - line:20:1]   'GenericS0'
// CHECK-EXPANDED-NEXT:   `-GenericParamScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:19:18 - line:20:1]   param 0 'T'
// CHECK-EXPANDED-NEXT:     `-GenericParamScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:19:21 - line:20:1]   param 1 'U'
// CHECK-EXPANDED-NEXT:       `-NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:19:24 - line:20:1]   'GenericS0'
// CHECK-EXPANDED-NEXT: |-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:22:1 - line:23:1]   'genericFunc0(t:u:i:)'
// CHECK-EXPANDED-NEXT:   `-GenericParamScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:22:19 - line:23:1]   param 0 'T'
// CHECK-EXPANDED-NEXT:     `-GenericParamScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:22:22 - line:23:1]   param 1 'U'
// CHECK-EXPANDED-NEXT:       `-AbstractFunctionParamsScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:22:24 - line:23:1]
// CHECK-EXPANDED-NEXT:         |-DefaultArgumentInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:22:46 - line:22:46]
// CHECK-EXPANDED-NEXT:         `-PureFunctionBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:22:50 - line:23:1]
// CHECK-EXPANDED-NEXT:           `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:22:50 - line:23:1]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:25:1 - line:31:1]   'ContainsGenerics0'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:25:25 - line:31:1]   'ContainsGenerics0'
// CHECK-EXPANDED-NEXT:     |-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:26:3 - line:27:3]   'init(t:u:)'
// CHECK-EXPANDED-NEXT:       `-GenericParamScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:26:8 - line:27:3]   param 0 'T'
// CHECK-EXPANDED-NEXT:         `-GenericParamScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:26:11 - line:27:3]   param 1 'U'
// CHECK-EXPANDED-NEXT:           `-AbstractFunctionParamsScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:26:13 - line:27:3]
// CHECK-EXPANDED-NEXT:             `-MethodBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:26:26 - line:27:3]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:26:26 - line:27:3]
// CHECK-EXPANDED-NEXT:     `-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:29:3 - line:30:3]   'deinit'
// CHECK-EXPANDED-NEXT:       `-AbstractFunctionParamsScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:29:3 - line:30:3]
// CHECK-EXPANDED-NEXT:         `-MethodBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:29:10 - line:30:3]
// CHECK-EXPANDED-NEXT:           `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:29:10 - line:30:3]
// CHECK-EXPANDED-NEXT: |-TypeAliasDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:33:1 - line:33:32]   <no extended nominal?!>
// CHECK-EXPANDED-NEXT:   `-GenericParamScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:33:25 - line:33:32]   param 0 'T'
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:38:1 - line:38:26]   'OtherArchStruct'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:38:24 - line:38:26]   'OtherArchStruct'
// CHECK-EXPANDED-NEXT: |-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:41:1 - line:100:1]   'functionBodies1(a:b:)'
// CHECK-EXPANDED-NEXT:   `-AbstractFunctionParamsScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:41:21 - line:100:1]
// CHECK-EXPANDED-NEXT:     `-PureFunctionBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:41:39 - line:100:1]
// CHECK-EXPANDED-NEXT:       `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:41:39 - line:100:1]
// CHECK-EXPANDED-NEXT:         `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:42:7 - line:99:38]   entry 0 'x1' 'x2'
// CHECK-EXPANDED-NEXT:           |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:42:18 - line:42:23]   entry 0 'x1' 'x2'
// CHECK-EXPANDED-NEXT:           `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:42:23 - line:99:38]   entry 0 'x1' 'x2'
// CHECK-EXPANDED-NEXT:             `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:43:7 - line:99:38]   entry 1 'y1' 'y2'
// CHECK-EXPANDED-NEXT:               |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:43:18 - line:43:23]   entry 1 'y1' 'y2'
// CHECK-EXPANDED-NEXT:               `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:43:23 - line:99:38]   entry 1 'y1' 'y2'
// CHECK-EXPANDED-NEXT:                 `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:44:7 - line:99:38]   entry 0 'z1' 'z2'
// CHECK-EXPANDED-NEXT:                   |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:44:18 - line:44:23]   entry 0 'z1' 'z2'
// CHECK-EXPANDED-NEXT:                   `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:44:23 - line:99:38]   entry 0 'z1' 'z2'
// CHECK-EXPANDED-NEXT:                     |-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:45:6 - line:52:3]
// CHECK-EXPANDED-NEXT:                       `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:46:9 - line:51:5]   entry 0 'a1'
// CHECK-EXPANDED-NEXT:                         |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:46:14 - line:46:14]   entry 0 'a1'
// CHECK-EXPANDED-NEXT:                         `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:46:14 - line:51:5]   entry 0 'a1'
// CHECK-EXPANDED-NEXT:                           `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:47:9 - line:51:5]   entry 0 'a2'
// CHECK-EXPANDED-NEXT:                             |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:47:14 - line:47:14]   entry 0 'a2'
// CHECK-EXPANDED-NEXT:                             `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:47:14 - line:51:5]   entry 0 'a2'
// CHECK-EXPANDED-NEXT:                               `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:48:8 - line:51:5]
// CHECK-EXPANDED-NEXT:                                 `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:49:11 - line:50:16]   entry 0 'b1'
// CHECK-EXPANDED-NEXT:                                   |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:49:16 - line:49:16]   entry 0 'b1'
// CHECK-EXPANDED-NEXT:                                   `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:49:16 - line:50:16]   entry 0 'b1'
// CHECK-EXPANDED-NEXT:                                     `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:50:11 - line:50:16]   entry 0 'b2'
// CHECK-EXPANDED-NEXT:                                       |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:50:16 - line:50:16]   entry 0 'b2'
// CHECK-EXPANDED-NEXT:                                       `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:50:16 - line:50:16]   entry 0 'b2'
// CHECK-EXPANDED-NEXT:                     |-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:53:6 - line:56:3]
// CHECK-EXPANDED-NEXT:                       `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:54:9 - line:55:14]   entry 0 'b1'
// CHECK-EXPANDED-NEXT:                         |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:54:14 - line:54:14]   entry 0 'b1'
// CHECK-EXPANDED-NEXT:                         `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:54:14 - line:55:14]   entry 0 'b1'
// CHECK-EXPANDED-NEXT:                           `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:55:9 - line:55:14]   entry 0 'b2'
// CHECK-EXPANDED-NEXT:                             |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:55:14 - line:55:14]   entry 0 'b2'
// CHECK-EXPANDED-NEXT:                             `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:55:14 - line:55:14]   entry 0 'b2'
// CHECK-EXPANDED-NEXT:                     |-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:57:3 - line:57:38]   'f(_:)'
// CHECK-EXPANDED-NEXT:                       `-AbstractFunctionParamsScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:57:9 - line:57:38]
// CHECK-EXPANDED-NEXT:                         `-PureFunctionBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:57:27 - line:57:38]
// CHECK-EXPANDED-NEXT:                           `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:57:27 - line:57:38]
// CHECK-EXPANDED-NEXT:                     `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:58:7 - line:99:38]   entry 0 'f2'
// CHECK-EXPANDED-NEXT:                       |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:58:12 - line:58:16]   entry 0 'f2'
// CHECK-EXPANDED-NEXT:                       `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:58:16 - line:99:38]   entry 0 'f2'
// CHECK-EXPANDED-NEXT:                         |-NominalTypeDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:59:3 - line:59:15]   'S7'
// CHECK-EXPANDED-NEXT:                           `-NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:59:13 - line:59:15]   'S7'
// CHECK-EXPANDED-NEXT:                         |-TypeAliasDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:60:3 - line:60:23]   <no extended nominal?!>
// CHECK-EXPANDED-NEXT:                         |-IfStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:62:3 - line:66:3]
// CHECK-EXPANDED-NEXT:                           |-ConditionalClauseScope, [SOURCE_DIR/test/NameBinding/scope_map.swift:62:6 - line:64:3]   in [SOURCE_DIR/test/NameBinding/scope_map.swift:62:3 - line:66:3] index 0
// CHECK-EXPANDED-NEXT:                             `-ConditionalClausePatternUseScope, [SOURCE_DIR/test/NameBinding/scope_map.swift:62:18 - line:64:3]  let b1
// CHECK-EXPANDED-NEXT:                               `-ConditionalClauseScope, [SOURCE_DIR/test/NameBinding/scope_map.swift:62:18 - line:64:3]   in [SOURCE_DIR/test/NameBinding/scope_map.swift:62:3 - line:66:3] index 1
// CHECK-EXPANDED-NEXT:                                 `-ConditionalClausePatternUseScope, [SOURCE_DIR/test/NameBinding/scope_map.swift:62:29 - line:64:3]  let b2
// CHECK-EXPANDED-NEXT:                                   `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:62:29 - line:64:3]
// CHECK-EXPANDED-NEXT:                                     `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:63:9 - line:63:14]   entry 0 'c1'
// CHECK-EXPANDED-NEXT:                                       |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:63:14 - line:63:14]   entry 0 'c1'
// CHECK-EXPANDED-NEXT:                                       `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:63:14 - line:63:14]   entry 0 'c1'
// CHECK-EXPANDED-NEXT:                           `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:64:10 - line:66:3]
// CHECK-EXPANDED-NEXT:                             `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:65:9 - line:65:14]   entry 0 'c2'
// CHECK-EXPANDED-NEXT:                               |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:65:14 - line:65:14]   entry 0 'c2'
// CHECK-EXPANDED-NEXT:                               `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:65:14 - line:65:14]   entry 0 'c2'
// CHECK-EXPANDED-NEXT:                         `-GuardStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:68:3 - line:99:38]
// CHECK-EXPANDED-NEXT:                           |-ConditionalClauseScope, [SOURCE_DIR/test/NameBinding/scope_map.swift:68:9 - line:68:53]   in [SOURCE_DIR/test/NameBinding/scope_map.swift:68:3 - line:71:3] index 0
// CHECK-EXPANDED-NEXT:                             `-ConditionalClausePatternUseScope, [SOURCE_DIR/test/NameBinding/scope_map.swift:68:21 - line:68:53]  let b1
// CHECK-EXPANDED-NEXT:                               `-ConditionalClauseScope, [SOURCE_DIR/test/NameBinding/scope_map.swift:68:21 - line:68:53]   in [SOURCE_DIR/test/NameBinding/scope_map.swift:68:3 - line:71:3] index 1
// CHECK-EXPANDED-NEXT:                                 |-WholeClosureScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:68:21 - line:68:30]
// CHECK-EXPANDED-NEXT:                                   `-ClosureBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:68:21 - line:68:30]
// CHECK-EXPANDED-NEXT:                                     `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:68:21 - line:68:30]
// CHECK-EXPANDED-NEXT:                                 `-ConditionalClauseScope, [SOURCE_DIR/test/NameBinding/scope_map.swift:68:37 - line:68:53]   in [SOURCE_DIR/test/NameBinding/scope_map.swift:68:3 - line:71:3] index 2
// CHECK-EXPANDED-NEXT:                                   `-ConditionalClausePatternUseScope, [SOURCE_DIR/test/NameBinding/scope_map.swift:68:53 - line:68:53]  let b2
// CHECK-EXPANDED-NEXT:                           |-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:68:53 - line:71:3]
// CHECK-EXPANDED-NEXT:                             `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:69:9 - line:69:13]   entry 0 'c'
// CHECK-EXPANDED-NEXT:                               |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:69:13 - line:69:13]   entry 0 'c'
// CHECK-EXPANDED-NEXT:                               `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:69:13 - line:69:13]   entry 0 'c'
// CHECK-EXPANDED-NEXT:                           `-GuardStmtUseScope, [SOURCE_DIR/test/NameBinding/scope_map.swift:71:3 - line:99:38]
// CHECK-EXPANDED-NEXT:                             |-WhileStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:73:3 - line:75:3]
// CHECK-EXPANDED-NEXT:                               `-ConditionalClauseScope, [SOURCE_DIR/test/NameBinding/scope_map.swift:73:9 - line:75:3]   in [SOURCE_DIR/test/NameBinding/scope_map.swift:73:3 - line:75:3] index 0
// CHECK-EXPANDED-NEXT:                                 `-ConditionalClausePatternUseScope, [SOURCE_DIR/test/NameBinding/scope_map.swift:73:21 - line:75:3]  let b3
// CHECK-EXPANDED-NEXT:                                   `-ConditionalClauseScope, [SOURCE_DIR/test/NameBinding/scope_map.swift:73:21 - line:75:3]   in [SOURCE_DIR/test/NameBinding/scope_map.swift:73:3 - line:75:3] index 1
// CHECK-EXPANDED-NEXT:                                     `-ConditionalClausePatternUseScope, [SOURCE_DIR/test/NameBinding/scope_map.swift:73:32 - line:75:3]  let b4
// CHECK-EXPANDED-NEXT:                                       `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:73:32 - line:75:3]
// CHECK-EXPANDED-NEXT:                                         `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:74:9 - line:74:13]   entry 0 'c'
// CHECK-EXPANDED-NEXT:                                           |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:74:13 - line:74:13]   entry 0 'c'
// CHECK-EXPANDED-NEXT:                                           `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:74:13 - line:74:13]   entry 0 'c'
// CHECK-EXPANDED-NEXT:                             |-RepeatWhileScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:77:3 - line:77:20]
// CHECK-EXPANDED-NEXT:                               `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:77:10 - line:77:12]
// CHECK-EXPANDED-NEXT:                             |-ForEachStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:79:3 - line:81:3]
// CHECK-EXPANDED-NEXT:                               `-ForEachPatternScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:79:52 - line:81:3]
// CHECK-EXPANDED-NEXT:                                 `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:79:63 - line:81:3]
// CHECK-EXPANDED-NEXT:                             |-DoCatchStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:83:3 - line:87:3]
// CHECK-EXPANDED-NEXT:                               |-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:83:6 - line:85:3]
// CHECK-EXPANDED-NEXT:                               |-CatchStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:85:31 - line:86:3]
// CHECK-EXPANDED-NEXT:                                 `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:85:54 - line:86:3]
// CHECK-EXPANDED-NEXT:                               `-CatchStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:86:11 - line:87:3]
// CHECK-EXPANDED-NEXT:                                 `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:86:11 - line:87:3]
// CHECK-EXPANDED-NEXT:                             |-SwitchStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:89:3 - line:98:3]
// CHECK-EXPANDED-NEXT:                               |-CaseStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:90:29 - line:91:10]
// CHECK-EXPANDED-NEXT:                                 `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:91:5 - line:91:10]
// CHECK-EXPANDED-NEXT:                               |-CaseStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:94:5 - line:94:10]
// CHECK-EXPANDED-NEXT:                                 `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:94:5 - line:94:10]
// CHECK-EXPANDED-NEXT:                               `-CaseStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:97:5 - line:97:10]
// CHECK-EXPANDED-NEXT:                                 `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:97:5 - line:97:10]
// CHECK-EXPANDED-NEXT:                             `-ForEachStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:99:3 - line:99:38]
// CHECK-EXPANDED-NEXT:                               `-ForEachPatternScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:99:36 - line:99:38]
// CHECK-EXPANDED-NEXT:                                 `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:99:36 - line:99:38]
// CHECK-EXPANDED-NEXT: |-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:102:1 - line:102:26]   'throwing()'
// CHECK-EXPANDED-NEXT:   `-AbstractFunctionParamsScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:102:14 - line:102:26]
// CHECK-EXPANDED-NEXT:     `-PureFunctionBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:102:24 - line:102:26]
// CHECK-EXPANDED-NEXT:       `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:102:24 - line:102:26]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:104:1 - line:106:1]   'MyError'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:104:24 - line:106:1]   'MyError'
// CHECK-EXPANDED-NEXT:     `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:105:7 - line:105:14]   entry 0 'value'
// CHECK-EXPANDED-NEXT:       `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:105:14 - line:105:14]   entry 0 'value'
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:108:1 - line:112:1]   'MyEnum'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:108:13 - line:112:1]   'MyEnum'
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:114:1 - line:130:1]   'StructContainsAbstractStorageDecls'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:114:43 - line:130:1]   'StructContainsAbstractStorageDecls'
// CHECK-EXPANDED-NEXT:     |-SubscriptDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:115:3 - line:121:3]   scope_map.(file).StructContainsAbstractStorageDecls.subscript(_:_:)@SOURCE_DIR/test/NameBinding/scope_map.swift:115:3
// CHECK-EXPANDED-NEXT:       `-AbstractFunctionParamsScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:115:13 - line:121:3]
// CHECK-EXPANDED-NEXT:         |-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:116:5 - line:117:5]   '_'
// CHECK-EXPANDED-NEXT:           `-MethodBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:116:9 - line:117:5]
// CHECK-EXPANDED-NEXT:             `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:116:9 - line:117:5]
// CHECK-EXPANDED-NEXT:         `-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:118:5 - line:120:5]   '_'
// CHECK-EXPANDED-NEXT:           `-MethodBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:118:9 - line:120:5]
// CHECK-EXPANDED-NEXT:             `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:118:9 - line:120:5]
// CHECK-EXPANDED-NEXT:     `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:123:7 - line:129:3]   entry 0 'computed'
// CHECK-EXPANDED-NEXT:       `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:123:17 - line:129:3]   entry 0 'computed'
// CHECK-EXPANDED-NEXT:         `-VarDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:123:21 - line:129:3]   scope_map.(file).StructContainsAbstractStorageDecls.computed@SOURCE_DIR/test/NameBinding/scope_map.swift:123:7
// CHECK-EXPANDED-NEXT:           |-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:124:5 - line:126:5]   '_'
// CHECK-EXPANDED-NEXT:             `-MethodBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:124:9 - line:126:5]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:124:9 - line:126:5]
// CHECK-EXPANDED-NEXT:           `-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:127:5 - line:128:5]   '_'
// CHECK-EXPANDED-NEXT:             `-MethodBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:127:9 - line:128:5]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:127:9 - line:128:5]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:132:1 - line:140:1]   'ClassWithComputedProperties'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:132:35 - line:140:1]   'ClassWithComputedProperties'
// CHECK-EXPANDED-NEXT:     |-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:133:7 - line:135:3]   entry 0 'willSetProperty'
// CHECK-EXPANDED-NEXT:       |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:133:30 - line:133:30]   entry 0 'willSetProperty'
// CHECK-EXPANDED-NEXT:       `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:133:30 - line:135:3]   entry 0 'willSetProperty'
// CHECK-EXPANDED-NEXT:         `-VarDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:133:32 - line:135:3]   scope_map.(file).ClassWithComputedProperties.willSetProperty@SOURCE_DIR/test/NameBinding/scope_map.swift:133:7
// CHECK-EXPANDED-NEXT:           `-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:134:5 - line:134:15]   '_'
// CHECK-EXPANDED-NEXT:             `-MethodBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:134:13 - line:134:15]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:134:13 - line:134:15]
// CHECK-EXPANDED-NEXT:     `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:137:7 - line:139:3]   entry 0 'didSetProperty'
// CHECK-EXPANDED-NEXT:       |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:137:29 - line:137:29]   entry 0 'didSetProperty'
// CHECK-EXPANDED-NEXT:       `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:137:29 - line:139:3]   entry 0 'didSetProperty'
// CHECK-EXPANDED-NEXT:         `-VarDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:137:31 - line:139:3]   scope_map.(file).ClassWithComputedProperties.didSetProperty@SOURCE_DIR/test/NameBinding/scope_map.swift:137:7
// CHECK-EXPANDED-NEXT:           `-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:138:5 - line:138:14]   '_'
// CHECK-EXPANDED-NEXT:             `-MethodBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:138:12 - line:138:14]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:138:12 - line:138:14]
// CHECK-EXPANDED-NEXT: |-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:142:1 - line:155:1]   'funcWithComputedProperties(i:)'
// CHECK-EXPANDED-NEXT:   `-AbstractFunctionParamsScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:142:32 - line:155:1]
// CHECK-EXPANDED-NEXT:     `-PureFunctionBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:142:41 - line:155:1]
// CHECK-EXPANDED-NEXT:       `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:142:41 - line:155:1]
// CHECK-EXPANDED-NEXT:         `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:143:7 - line:154:8]   entry 0 'computed'
// CHECK-EXPANDED-NEXT:           `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:143:17 - line:154:8]   entry 0 'computed'
// CHECK-EXPANDED-NEXT:             |-VarDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:143:21 - line:149:3]   scope_map.(file).funcWithComputedProperties(i:).computed@SOURCE_DIR/test/NameBinding/scope_map.swift:143:7
// CHECK-EXPANDED-NEXT:               |-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:144:5 - line:145:5]   '_'
// CHECK-EXPANDED-NEXT:                 `-PureFunctionBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:144:9 - line:145:5]
// CHECK-EXPANDED-NEXT:                   `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:144:9 - line:145:5]
// CHECK-EXPANDED-NEXT:               `-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:146:5 - line:148:5]   '_'
// CHECK-EXPANDED-NEXT:                 `-PureFunctionBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:146:9 - line:148:5]
// CHECK-EXPANDED-NEXT:                   `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:146:9 - line:148:5]
// CHECK-EXPANDED-NEXT:             `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:149:6 - line:154:8]   entry 1 'stored1' 'stored2'
// CHECK-EXPANDED-NEXT:               |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:149:31 - line:149:36]   entry 1 'stored1' 'stored2'
// CHECK-EXPANDED-NEXT:               `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:149:36 - line:154:8]   entry 1 'stored1' 'stored2'
// CHECK-EXPANDED-NEXT:                 `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:150:3 - line:154:8]   entry 2 'alsoComputed'
// CHECK-EXPANDED-NEXT:                   `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:150:21 - line:154:8]   entry 2 'alsoComputed'
// CHECK-EXPANDED-NEXT:                     |-VarDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:150:25 - line:152:3]   scope_map.(file).funcWithComputedProperties(i:).alsoComputed@SOURCE_DIR/test/NameBinding/scope_map.swift:150:7
// CHECK-EXPANDED-NEXT:                       `-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:150:25 - line:152:3]   '_'
// CHECK-EXPANDED-NEXT:                         `-PureFunctionBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:150:25 - line:152:3]
// CHECK-EXPANDED-NEXT:                           `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:150:25 - line:152:3]
// CHECK-EXPANDED-NEXT:                     `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:154:6 - line:154:8]
// CHECK-EXPANDED-NEXT: |-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:157:1 - line:162:1]   'closures()'
// CHECK-EXPANDED-NEXT:   `-AbstractFunctionParamsScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:157:14 - line:162:1]
// CHECK-EXPANDED-NEXT:     `-PureFunctionBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:157:17 - line:162:1]
// CHECK-EXPANDED-NEXT:       `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:157:17 - line:162:1]
// CHECK-EXPANDED-NEXT:         |-WholeClosureScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:158:3 - line:160:3]
// CHECK-EXPANDED-NEXT:           `-ClosureParametersScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:158:5 - line:160:3]
// CHECK-EXPANDED-NEXT:             `-ClosureBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:158:10 - line:160:3]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:158:10 - line:160:3]
// CHECK-EXPANDED-NEXT:                 `-WholeClosureScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:159:12 - line:159:22]
// CHECK-EXPANDED-NEXT:                   `-ClosureBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:159:12 - line:159:22]
// CHECK-EXPANDED-NEXT:                     `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:159:12 - line:159:22]
// CHECK-EXPANDED-NEXT:         `-WholeClosureScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:161:3 - line:161:19]
// CHECK-EXPANDED-NEXT:           `-ClosureParametersScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:161:5 - line:161:19]
// CHECK-EXPANDED-NEXT:             `-ClosureBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:161:10 - line:161:19]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:161:10 - line:161:19]
// CHECK-EXPANDED-NEXT: `-TopLevelCodeScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:164:1 - line:194:1]
// CHECK-EXPANDED-NEXT:   `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:164:1 - line:194:1]
// CHECK-EXPANDED-NEXT:     |-WholeClosureScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:164:1 - line:164:14]
// CHECK-EXPANDED-NEXT:       `-ClosureBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:164:1 - line:164:14]
// CHECK-EXPANDED-NEXT:         `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:164:1 - line:164:14]
// CHECK-EXPANDED-NEXT:     |-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:166:1 - line:175:1]   'defaultArguments(i:j:)'
// CHECK-EXPANDED-NEXT:       `-AbstractFunctionParamsScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:166:22 - line:175:1]
// CHECK-EXPANDED-NEXT:         |-DefaultArgumentInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:166:32 - line:166:32]
// CHECK-EXPANDED-NEXT:         |-DefaultArgumentInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:167:32 - line:167:48]
// CHECK-EXPANDED-NEXT:           `-WholeClosureScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:167:32 - line:167:42]
// CHECK-EXPANDED-NEXT:             `-ClosureBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:167:32 - line:167:42]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:167:32 - line:167:42]
// CHECK-EXPANDED-NEXT:         `-PureFunctionBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:167:51 - line:175:1]
// CHECK-EXPANDED-NEXT:           `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:167:51 - line:175:1]
// CHECK-EXPANDED-NEXT:             |-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:169:3 - line:171:3]   'localWithDefaults(i:j:)'
// CHECK-EXPANDED-NEXT:               `-AbstractFunctionParamsScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:169:25 - line:171:3]
// CHECK-EXPANDED-NEXT:                 |-DefaultArgumentInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:169:35 - line:169:35]
// CHECK-EXPANDED-NEXT:                 |-DefaultArgumentInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:170:35 - line:170:51]
// CHECK-EXPANDED-NEXT:                   `-WholeClosureScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:170:35 - line:170:45]
// CHECK-EXPANDED-NEXT:                     `-ClosureBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:170:35 - line:170:45]
// CHECK-EXPANDED-NEXT:                       `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:170:35 - line:170:45]
// CHECK-EXPANDED-NEXT:                 `-PureFunctionBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:170:54 - line:171:3]
// CHECK-EXPANDED-NEXT:                   `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:170:54 - line:171:3]
// CHECK-EXPANDED-NEXT:             `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:173:7 - line:174:11]   entry 0 'a'
// CHECK-EXPANDED-NEXT:               |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:173:11 - line:174:11]   entry 0 'a'
// CHECK-EXPANDED-NEXT:                 `-WholeClosureScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:174:3 - line:174:8]
// CHECK-EXPANDED-NEXT:                   `-ClosureBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:174:3 - line:174:8]
// CHECK-EXPANDED-NEXT:                     `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:174:3 - line:174:8]
// CHECK-EXPANDED-NEXT:               `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:174:11 - line:174:11]   entry 0 'a'
// CHECK-EXPANDED-NEXT:     |-NominalTypeDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:177:1 - line:180:1]   'PatternInitializers'
// CHECK-EXPANDED-NEXT:       `-NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:177:28 - line:180:1]   'PatternInitializers'
// CHECK-EXPANDED-NEXT:         `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:178:7 - line:179:25]   entry 0 'a' 'b'
// CHECK-EXPANDED-NEXT:           |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:178:16 - line:178:21]   entry 0 'a' 'b'
// CHECK-EXPANDED-NEXT:           `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:178:21 - line:179:25]   entry 0 'a' 'b'
// CHECK-EXPANDED-NEXT:             `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:179:7 - line:179:25]   entry 1 'c' 'd'
// CHECK-EXPANDED-NEXT:               |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:179:16 - line:179:25]   entry 1 'c' 'd'
// CHECK-EXPANDED-NEXT:               `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:179:25 - line:179:25]   entry 1 'c' 'd'
// CHECK-EXPANDED-NEXT:     |-NominalTypeDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:182:1 - line:184:1]   'ProtoWithSubscript'
// CHECK-EXPANDED-NEXT:       `-GenericParamScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:182:29 - line:184:1]   param 0 'Self : ProtoWithSubscript'
// CHECK-EXPANDED-NEXT:         `-NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:182:29 - line:184:1]   'ProtoWithSubscript'
// CHECK-EXPANDED-NEXT:           `-SubscriptDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:183:3 - line:183:43]   scope_map.(file).ProtoWithSubscript.subscript(_:)@SOURCE_DIR/test/NameBinding/scope_map.swift:183:3
// CHECK-EXPANDED-NEXT:             `-AbstractFunctionParamsScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:183:12 - line:183:43]
// CHECK-EXPANDED-NEXT:               |-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:183:35 - line:183:35]   '_'
// CHECK-EXPANDED-NEXT:               `-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:183:39 - line:183:39]   '_'
// CHECK-EXPANDED-NEXT:     |-AbstractFunctionDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:186:1 - line:188:1]   'localPatternsWithSharedType()'
// CHECK-EXPANDED-NEXT:       `-AbstractFunctionParamsScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:186:33 - line:188:1]
// CHECK-EXPANDED-NEXT:         `-PureFunctionBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:186:36 - line:188:1]
// CHECK-EXPANDED-NEXT:           `-BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:186:36 - line:188:1]
// CHECK-EXPANDED-NEXT:             `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:187:7 - line:187:16]   entry 0 'i'
// CHECK-EXPANDED-NEXT:               `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:187:7 - line:187:16]   entry 0 'i'
// CHECK-EXPANDED-NEXT:                 `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:187:10 - line:187:16]   entry 1 'j'
// CHECK-EXPANDED-NEXT:                   `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:187:10 - line:187:16]   entry 1 'j'
// CHECK-EXPANDED-NEXT:                     `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:187:13 - line:187:16]   entry 2 'k'
// CHECK-EXPANDED-NEXT:                       `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:187:16 - line:187:16]   entry 2 'k'
// CHECK-EXPANDED-NEXT:     `-NominalTypeDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:190:1 - line:194:1]   'LazyProperties'
// CHECK-EXPANDED-NEXT:       `-NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:190:22 - line:194:1]   'LazyProperties'
// CHECK-EXPANDED-NEXT:         |-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:191:7 - line:191:20]   entry 0 'value'
// CHECK-EXPANDED-NEXT:           |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:191:20 - line:191:20]   entry 0 'value'
// CHECK-EXPANDED-NEXT:           `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:191:20 - line:191:20]   entry 0 'value'
// CHECK-EXPANDED-NEXT:         `-PatternEntryDeclScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:193:12 - line:193:29]   entry 0 'prop'
// CHECK-EXPANDED-NEXT:           |-PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:193:24 - line:193:29]   entry 0 'prop'
// CHECK-EXPANDED-NEXT:           `-PatternEntryUseScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:193:29 - line:193:29]   entry 0 'prop'





// RUN: not %target-swift-frontend -dump-scope-maps 70:8,26:20,5:18,166:32,179:18,193:26 %s 2> %t.searches
// RUN: %FileCheck -check-prefix CHECK-SEARCHES %s < %t.searches


// CHECK-SEARCHES:      ***Scope at 70:8***
// CHECK-SEARCHES-NEXT: BraceStmtScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:68:53 - line:71:3]
// CHECK-SEARCHES-NEXT: ***Scope at 26:20***
// CHECK-SEARCHES-NEXT: AbstractFunctionParamsScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:26:13 - line:27:3]
// CHECK-SEARCHES-NEXT: ***Scope at 5:18***
// CHECK-SEARCHES-NEXT: NominalTypeBodyScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:5:17 - line:5:19]   'InnerC0'
// CHECK-SEARCHES-NEXT: {{.*}} Module name=scope_map
// CHECK-SEARCHES-NEXT:   {{.*}} FileUnit file="SOURCE_DIR/test/NameBinding/scope_map.swift"
// CHECK-SEARCHES-NEXT:     {{.*}} StructDecl name=S0
// CHECK-SEARCHES-NEXT:       {{.*}} ClassDecl name=InnerC0
// CHECK-SEARCHES-NEXT: ***Scope at 166:32***
// CHECK-SEARCHES-NEXT: DefaultArgumentInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:166:32 - line:166:32]
// CHECK-SEARCHES-NEXT: {{.*}} Module name=scope_map
// CHECK-SEARCHES-NEXT:   {{.*}} FileUnit file="SOURCE_DIR/test/NameBinding/scope_map.swift"
// CHECK-SEARCHES-NEXT:     {{.*}} AbstractFunctionDecl name=defaultArguments(i:j:) : (Int, Int) -> ()
// CHECK-SEARCHES-NEXT:       {{.*}} Initializer DefaultArgument index=0
// CHECK-SEARCHES-NEXT: ***Scope at 179:18***
// CHECK-SEARCHES-NEXT: PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:179:16 - line:179:25]   entry 1 'c' 'd'
// CHECK-SEARCHES-NEXT: {{.*}} Module name=scope_map
// CHECK-SEARCHES-NEXT:   {{.*}} FileUnit file="SOURCE_DIR/test/NameBinding/scope_map.swift"
// CHECK-SEARCHES-NEXT:     {{.*}} StructDecl name=PatternInitializers
// CHECK-SEARCHES-NEXT:       {{.*}} Initializer PatternBinding {{.*}}18 #1
// CHECK-SEARCHES-NEXT: ***Scope at 193:26***
// CHECK-SEARCHES-NEXT: PatternEntryInitializerScope {{.*}}, [SOURCE_DIR/test/NameBinding/scope_map.swift:193:24 - line:193:29]   entry 0 'prop'
// CHECK-SEARCHES-NEXT: {{.*}} Module name=scope_map
// CHECK-SEARCHES-NEXT:   {{.*}} FileUnit file="SOURCE_DIR/test/NameBinding/scope_map.swift"
// CHECK-SEARCHES-NEXT:     {{.*}} ClassDecl name=LazyProperties
// CHECK-SEARCHES-NEXT:       {{.*}} Initializer PatternBinding {{.*}}80 #0
// CHECK-SEARCHES-NEXT: Local bindings: self



// CHECK-SEARCHES-NOT:  ***Complete scope map***
