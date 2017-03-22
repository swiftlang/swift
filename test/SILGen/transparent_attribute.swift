// RUN: %target-swift-frontend -emit-silgen -emit-verbose-sil %s | %FileCheck %s

// Test that the attribute gets set on default argument generators.
@_transparent func transparentFuncWithDefaultArgument (x: Int = 1) -> Int {
  return x
}
func useTransparentFuncWithDefaultArgument() -> Int {
  return transparentFuncWithDefaultArgument();

  // CHECK-LABEL: sil hidden @_T021transparent_attribute37useTransparentFuncWithDefaultArgumentSiyF
  // CHECK: apply {{.*}} line:8:44
  // CHECK: apply {{.*}} line:8:10
  // CHECK: return
  
}

func transparentFuncWithoutDefaultArgument (x: Int = 1) -> Int {
  return x
}
func useTransparentFuncWithoutDefaultArgument() -> Int {
  return transparentFuncWithoutDefaultArgument();

  // CHECK-LABEL: sil hidden @_T021transparent_attribute40useTransparentFuncWithoutDefaultArgumentSiyF
  // CHECK: apply {{.*}} line:21:47
  // CHECK-NOT: transparent
  // CHECK: apply {{.*}} line:21:10
  // CHECK: return
  
}

// Make sure the transparent attribute is set on constructors (allocating and initializing).
struct StructWithTranspConstructor {
  @_transparent init () {}
}
func testStructWithTranspConstructor() -> StructWithTranspConstructor {
  return StructWithTranspConstructor()
  
  // transparent_attribute.StructWithTranspConstructor.constructor
  // CHECK-APPLY: sil hidden @_T021transparent_attribute27StructWithTranspConstructorV{{[_0-9a-zA-Z]*}}fC
  
  // testStructWithTranspConstructor
  // CHECK-APPLY: _T21transparent_attribute31testStructWithTranspConstructorFT_VS_27StructWithTranspConstructor
  // CHECK: apply {{.*}} line:36:10
  
}

struct MySt {}
var _x = MySt()
var x1 : MySt {
  @_transparent
  get {
    return _x
  }
  @_transparent
  set {
    _x = newValue
  }
}
var x2 : MySt {
  @_transparent
  set(v) {
    _x = v
  }
  @_transparent
  get {
    return _x
  }
}
func testProperty(z: MySt) {
  x1 = z
  x2 = z
  var m1 : MySt = x1
  var m2 : MySt = x2
  // CHECK-APPLY: sil hidden @_T021transparent_attribute12testPropertyyAA4MyStV1z_tF
  // CHECK: function_ref @_T021transparent_attribute2x1AA4MyStVfs
  // CHECK-NEXT: apply
  // CHECK: function_ref @_T021transparent_attribute2x2AA4MyStVfs
  // CHECK-NEXT: apply
  // CHECK: function_ref @_T021transparent_attribute2x1AA4MyStVfg
  // CHECK-NEXT: apply
  // CHECK: function_ref @_T021transparent_attribute2x2AA4MyStVfg
  // CHECK-NEXT: apply
}

var _tr2 = MySt()
var _tr3 = MySt()
struct MyTranspStruct {}
extension MyTranspStruct {
  @_transparent
  init(input : MySt) {}
  mutating
  func tr1() {}
  var tr2: MySt {
    get {
      return _tr2
    }
    set {
      _tr2 = newValue
    }
  }
}

extension MyTranspStruct {
    @_transparent
    var tr3: MySt {
    get {
      return _tr3
    }
    set {
      _tr3 = newValue
    }
  }
}

func testStructExtension() {
  var c : MyTranspStruct = MyTranspStruct(input: _x)
  c.tr1()
  var s : MySt = c.tr2
  var t : MySt = c.tr3
  // CHECK-APPLY: sil hidden @_TF21transparent_attribute13testStructExtensionFT_T_
  // CHECK: [[INIT:%[0-9]+]] = function_ref @_T021transparent_attribute14MyTranspStructV{{[_0-9a-zA-Z]*}}fC
  // CHECK: apply [[INIT]]
  // CHECK: [[TR1:%[0-9]+]] = function_ref @_T021transparent_attribute14MyTranspStructV3tr1{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[TR1]]
  // CHECK: [[TR2:%[0-9]+]] = function_ref @_T021transparent_attribute14MyTranspStructV3tr2AA0C2StVfg
  // CHECK: apply [[TR2]]
  // CHECK: [[TR3:%[0-9]+]] = function_ref @_T021transparent_attribute14MyTranspStructV3tr3AA0C2StVfg
  // CHECK: apply [[TR3]]
}

enum MyEnum {
  case onetransp
  case twotransp
}

extension MyEnum {
  @_transparent
  func tr3() {}
}

func testEnumExtension() {
  MyEnum.onetransp.tr3()
  // CHECK-APPLY: sil hidden @_T021transparent_attribute17testEnumExtensionyyF
  // CHECK: [[TR3:%[0-9]+]] = function_ref @_T021transparent_attribute6MyEnumO3tr3{{[_0-9a-zA-Z]*}}F
  // CHECK: [[INIT:%[0-9]+]] = enum $MyEnum, #MyEnum.onetransp!enumelt
  // CHECK: apply [[TR3]]([[INIT]])
}

struct testVarDecl {
  @_transparent var max: Int {
    get {
      return 0xFF
    }
    mutating
    set {
      max = 0xFF
    }
  }
  func testVarDeclFoo () {
    var z: Int = max
    // CHECK-APPLY: sil hidden @_T021transparent_attribute11testVarDeclV0cdE3Foo{{[_0-9a-zA-Z]*}}F
    // CHECK: [[TR4:%[0-9]+]] = function_ref @_T021transparent_attribute11testVarDeclV3maxSifg
    // CHECK: apply [[TR4]]
  }
}

struct testVarDeclShortenedSyntax {
  @_transparent static var max: Int { return 0xFF };
  func testVarDeclShortenedSyntaxfoo () {
    var z: Int = testVarDeclShortenedSyntax.max
    // CHECK-APPLY: sil hidden @_T021transparent_attribute26testVarDeclShortenedSyntaxV0cdeF9Syntaxfoo{{[_0-9a-zA-Z]*}}F
    // CHECK: [[TR5:%[0-9]+]] = function_ref @_T021transparent_attribute26testVarDeclShortenedSyntaxV3maxSifgZ
    // CHECK: apply [[TR5]]
  }
};

@_transparent var transparentOnGlobalVar: Int {
  get {
    return 0xFF
  }
}
// CHECK: sil hidden [transparent] @_T021transparent_attribute0A11OnGlobalVarSifg

// Local functions in transparent context are fragile.
@_transparent public func foo() {
  // CHECK-LABEL: sil shared [fragile] @_T021transparent_attribute3fooyyF3barL_yyF : $@convention(thin) () -> ()
  func bar() {}
  bar()

  // CHECK-LABEL: sil shared [fragile] @_T021transparent_attribute3fooyyFyycfU_ : $@convention(thin) () -> () {
  let f: () -> () = {}
  f()

  // CHECK-LABEL: sil shared [fragile] @_T021transparent_attribute3fooyyF3zimL_yyF : $@convention(thin) () -> () {
  func zim() {
    // CHECK-LABEL: sil shared [fragile] @_T021transparent_attribute3fooyyF3zimL_yyF4zangL_yyF : $@convention(thin) () -> () {
    func zang() {
    }
    zang()
  }
  zim()
}


// Check that @_versioned entities have public linkage.
// CHECK-LABEL: sil @_T021transparent_attribute25referencedFromTransparentyyF : $@convention(thin) () -> () {
@_versioned func referencedFromTransparent() {}

// CHECK-LABEL: sil [transparent] [fragile] @_T021transparent_attribute23referencesVersionedFuncyycyF : $@convention(thin) () -> @owned @callee_owned () -> () {
@_transparent public func referencesVersionedFunc() -> () -> () {
  return referencedFromTransparent
}
