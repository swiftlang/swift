// RUN: %target-swift-frontend -emit-silgen -emit-verbose-sil %s | %FileCheck %s

// Test if 'transparent' attribute gets propagated correctly to apply instructions.

// Test that the attribute gets set on default argument generators.
@_transparent func transparentFuncWithDefaultArgument (x: Int = 1) -> Int {
  return x
}
func useTransparentFuncWithDefaultArgument() -> Int {
  return transparentFuncWithDefaultArgument();

  // CHECK-LABEL: sil hidden @_TF21transparent_attribute37useTransparentFuncWithDefaultArgumentFT_Si
  // CHECK: apply {{.*}} line:10:44
  // CHECK: apply {{.*}} line:10:10
  // CHECK: return
  
}

func transparentFuncWithoutDefaultArgument (x: Int = 1) -> Int {
  return x
}
func useTransparentFuncWithoutDefaultArgument() -> Int {
  return transparentFuncWithoutDefaultArgument();

  // CHECK-LABEL: sil hidden @_TF21transparent_attribute40useTransparentFuncWithoutDefaultArgumentFT_Si
  // CHECK: apply {{.*}} line:23:47
  // CHECK-NOT: transparent
  // CHECK: apply {{.*}} line:23:10
  // CHECK: return
  
}

// Make sure the transparent attribute is set on constructors (allocating and initializing).
struct StructWithTranspConstructor {
  @_transparent init () {}
}
func testStructWithTranspConstructor() -> StructWithTranspConstructor {
  return StructWithTranspConstructor()
  
  // transparent_attribute.StructWithTranspConstructor.constructor
  // CHECK-APPLY: sil hidden @_TFV21transparent_attribute27StructWithTranspConstructorC
  
  // testStructWithTranspConstructor
  // CHECK-APPLY: _T21transparent_attribute31testStructWithTranspConstructorFT_VS_27StructWithTranspConstructor
  // CHECK: apply {{.*}} line:38:10
  
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
  // CHECK-APPLY: sil hidden @_TF21transparent_attribute12testPropertyFT1zVS_4MySt_T_
  // CHECK: function_ref @_TF21transparent_attributes2x1VS_4MySt
  // CHECK-NEXT: apply
  // CHECK: function_ref @_TF21transparent_attributes2x2VS_4MySt
  // CHECK-NEXT: apply
  // CHECK: function_ref @_TF21transparent_attributeg2x1VS_4MySt
  // CHECK-NEXT: apply
  // CHECK: function_ref @_TF21transparent_attributeg2x2VS_4MySt
  // CHECK-NEXT: apply
}

var _tr2 = MySt()
var _tr3 = MySt()
struct MyTranspStruct {}
@_transparent extension MyTranspStruct {
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
  // CHECK: [[INIT:%[0-9]+]] = function_ref @_TFV21transparent_attribute14MyTranspStructC
  // CHECK: apply [[INIT]]
  // CHECK: [[TR1:%[0-9]+]] = function_ref @_TFV21transparent_attribute14MyTranspStruct3tr1
  // CHECK: apply [[TR1]]
  // CHECK: [[TR2:%[0-9]+]] = function_ref @_TFV21transparent_attribute14MyTranspStructg3tr2VS_4MySt
  // CHECK: apply [[TR2]]
  // CHECK: [[TR3:%[0-9]+]] = function_ref @_TFV21transparent_attribute14MyTranspStructg3tr3VS_4MySt
  // CHECK: apply [[TR3]]
}

enum MyEnum {
  case onetransp
  case twotransp
}

@_transparent extension MyEnum {
  func tr3() {}
}

func testEnumExtension() {
  MyEnum.onetransp.tr3()
  // CHECK-APPLY: sil hidden @_TF21transparent_attribute17testEnumExtensionFT_T_
  // CHECK: [[TR3:%[0-9]+]] = function_ref @_TFO21transparent_attribute6MyEnum3tr3
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
    // CHECK-APPLY: sil hidden @_TFV21transparent_attribute11testVarDecl14testVarDeclFoo
    // CHECK: [[TR4:%[0-9]+]] = function_ref @_TFV21transparent_attribute11testVarDeclg3maxSi
    // CHECK: apply [[TR4]]
  }
}

struct testVarDeclShortenedSyntax {
  @_transparent static var max: Int { return 0xFF };
  func testVarDeclShortenedSyntaxfoo () {
    var z: Int = testVarDeclShortenedSyntax.max
    // CHECK-APPLY: sil hidden @_TFV21transparent_attribute26testVarDeclShortenedSyntax29testVarDeclShortenedSyntaxfoo
    // CHECK: [[TR5:%[0-9]+]] = function_ref @_TZFV21transparent_attribute26testVarDeclShortenedSyntaxg3maxSi
    // CHECK: apply [[TR5]]
  }
};

@_transparent var transparentOnGlobalVar: Int {
  get {
    return 0xFF
  }
}
// CHECK: sil hidden [transparent] @_TF21transparent_attributeg22transparentOnGlobalVarSi

// Local functions in transparent context are fragile.
@_transparent public func foo() {
  // CHECK-LABEL: sil shared [fragile] @_TFF21transparent_attribute3fooFT_T_L_3barFT_T_ : $@convention(thin) () -> ()
  func bar() {}
  bar()

  // CHECK-LABEL: sil shared [fragile] @_TFF21transparent_attribute3fooFT_T_U_FT_T_ : $@convention(thin) () -> () {
  let f: () -> () = {}
  f()

  // CHECK-LABEL: sil shared [fragile] @_TFF21transparent_attribute3fooFT_T_L_3zimFT_T_ : $@convention(thin) () -> () {
  func zim() {
    // CHECK-LABEL: sil shared [fragile] @_TFFF21transparent_attribute3fooFT_T_L_3zimFT_T_L_4zangFT_T_ : $@convention(thin) () -> () {
    func zang() {
    }
    zang()
  }
  zim()
}


// Check that @_versioned entities have public linkage.
// CHECK-LABEL: sil @_TF21transparent_attribute25referencedFromTransparentFT_T_ : $@convention(thin) () -> () {
@_versioned func referencedFromTransparent() {}

// CHECK-LABEL: sil [transparent] [fragile] @_TF21transparent_attribute23referencesVersionedFuncFT_FT_T_ : $@convention(thin) () -> @owned @callee_owned () -> () {
@_transparent public func referencesVersionedFunc() -> () -> () {
  return referencedFromTransparent
}
