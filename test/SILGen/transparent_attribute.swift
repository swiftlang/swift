// RUN: %swift -emit-silgen -emit-verbose-sil %s | FileCheck %s

// Test if 'transparent' atribute gets propagated correctly to apply instructions.

// Test that the attribute gets set on default argument generators.
@transparent func transparentFuncWithDefaultArgument (x: Int = 1) -> Int {
  return x
}
func useTransparentFuncWithDefaultArgument() ->Int {
  return transparentFuncWithDefaultArgument();

  // CHECK-LABEL: sil hidden @_TF21transparent_attribute37useTransparentFuncWithDefaultArgumentFT_Si
  // CHECK: apply [transparent] {{.*}} line:10:44
  // CHECK: apply [transparent] {{.*}} line:10:10
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
  @transparent init () {}
}
func testStructWithTranspConstructor() -> StructWithTranspConstructor {
  return StructWithTranspConstructor()
  
  // transparent_attribute.StructWithTranspConstructor.constructor
  // CHECK-APPLY: sil hidden @_TFV21transparent_attribute27StructWithTranspConstructorCfMS0_FT_S0_
  
  // testStructWithTranspConstructor
  // CHECK-APPLY: _T21transparent_attribute31testStructWithTranspConstructorFT_VS_27StructWithTranspConstructor
  // CHECK: apply [transparent] {{.*}} line:38:10
  
}

struct MySt {}
var _x = MySt()
var x1 : MySt {
  @transparent
  get {
    return _x
  }
  @transparent
  set {
    _x = newValue
  }
}
var x2 : MySt {
  @transparent
  set(v) {
    _x = v
  }
  @transparent
  get {
    return _x
  }
}
func testProperty(z: MySt) {
  x1 = z
  x2 = z
  var m1 : MySt = x1;
  var m2 : MySt = x2;
  // CHECK-APPLY: sil hidden @_TF21transparent_attribute12testPropertyFT1zVS_4MySt_T_
  // CHECK: function_ref @_TF21transparent_attributes2x1VS_4MySt
  // CHECK-NEXT: apply [transparent]
  // CHECK: function_ref @_TF21transparent_attributes2x2VS_4MySt
  // CHECK-NEXT: apply [transparent]
  // CHECK: function_ref @_TF21transparent_attributeg2x1VS_4MySt
  // CHECK-NEXT: apply [transparent]
  // CHECK: function_ref @_TF21transparent_attributeg2x2VS_4MySt
  // CHECK-NEXT: apply [transparent]
}

var _tr2 = MySt()
struct MyTranspStruct {}
@transparent extension MyTranspStruct {
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
func testStructExtension() {
  var c : MyTranspStruct = MyTranspStruct(input: _x)
  c.tr1()
  var s : MySt = c.tr2
  // CHECK-APPLY: sil hidden @_TF21transparent_attribute13testStructExtensionFT_T_
  // CHECK: [[INIT:%[0-9]+]] = function_ref @_TFV21transparent_attribute14MyTranspStructCfMS0_FT5inputVS_4MySt_S0_
  // CHECK: apply [transparent] [[INIT]]
  // CHECK: [[TR1:%[0-9]+]] = function_ref @_TFV21transparent_attribute14MyTranspStruct3tr1fRS0_FT_T_
  // CHECK: apply [transparent] [[TR1]]
  // CHECK: [[TR2:%[0-9]+]] = function_ref @_TFV21transparent_attribute14MyTranspStructg3tr2VS_4MySt
  // CHECK: apply [transparent] [[TR2]]
}

enum MyEnum {
  case onetransp
  case twotransp
}

@transparent extension MyEnum {
  func tr3() {}
}

func testEnumExtension() {
  MyEnum.onetransp.tr3()
  // CHECK-APPLY: sil hidden @_TF21transparent_attribute17testEnumExtensionFT_T_
  // CHECK: [[TR3:%[0-9]+]] = function_ref @_TFO21transparent_attribute6MyEnum3tr3fS0_FT_T_
  // CHECK: [[INIT:%[0-9]+]] = function_ref @_TFO21transparent_attribute6MyEnum9onetranspFMS0_S0_
  // CHECK: apply [transparent] [[INIT]]
  // CHECK: apply [transparent] [[TR3]]
}

struct testVarDecl {
  @transparent var max: Int {
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
    // CHECK-APPLY: sil hidden @_TFV21transparent_attribute11testVarDecl14testVarDeclFoofRS0_FT_T_
    // CHECK: [[TR4:%[0-9]+]] = function_ref @_TFV21transparent_attribute11testVarDeclg3maxSi
    // CHECK: apply [transparent] [[TR4]]
  }
}

struct testVarDeclShortenedSyntax {
  @transparent static var max: Int { return 0xFF };
  func testVarDeclShortenedSyntaxfoo () {
    var z: Int = testVarDeclShortenedSyntax.max
    // CHECK-APPLY: sil hidden @_TFV21transparent_attribute26testVarDeclShortenedSyntax29testVarDeclShortenedSyntaxfoofRS0_FT_T_
    // CHECK: [[TR5:%[0-9]+]] = function_ref @_TFV21transparent_attribute26testVarDeclShortenedSyntaxg3maxSi
    // CHECK: apply [transparent] [[TR5]]
  }
};

@transparent var transparentOnGlobalVar: Int {
  get {
    return 0xFF
  }
}
// CHECK: sil hidden [transparent] @_TF21transparent_attributeg22transparentOnGlobalVarSi

// Local functions in transparent context have public linkage.
@transparent func foo() {
  // CHECK-LABEL: sil @_TFF21transparent_attribute3fooFT_T_L_3barFT_T_ : $@thin () -> ()
  func bar() {}
  bar()

  // CHECK-LABEL: sil @_TFF21transparent_attribute3fooFT_T_U_FT_T_ : $@thin () -> () {
  let f: () -> () = {}
  f()

  // CHECK-LABEL: sil @_TFF21transparent_attribute3fooFT_T_L_3zimFT_T_ : $@thin () -> () {
  func zim() {
    // CHECK-LABEL: sil @_TFFF21transparent_attribute3fooFT_T_L_3zimFT_T_L_4zangFT_T_ : $@thin () -> () {
    func zang() {
    }
    zang()
  }
  zim()
}
