// This file should not have any syntax or type checker errors.
// RUN: %swift -parse %s

// RUN: %swift-ide-test -print-types -source-filename %s -fully-qualified-types=false | FileCheck %s -strict-whitespace
// RUN: %swift-ide-test -print-types -source-filename %s -fully-qualified-types=true | FileCheck %s -check-prefix=FULL -strict-whitespace

typealias MyInt = Int
// CHECK: TypeAliasDecl '''MyInt''' MyInt.metatype{{$}}
// FULL:  TypeAliasDecl '''MyInt''' swift_ide_test.MyInt.metatype{{$}}

func testVariableTypes(param: Int) {
// CHECK: FuncDecl '''testVariableTypes''' (param : Int) -> (){{$}}
// FULL:  FuncDecl '''testVariableTypes''' (param : Int) -> (){{$}}

  var a1 = 42
// CHECK: VarDecl '''a1''' Int{{$}}
// CHECK:         IntegerLiteralExpr:[[@LINE-2]] '''42''' Int64{{$}}
// FULL:  VarDecl '''a1''' swift.Int{{$}}
// FULL:          IntegerLiteralExpr:[[@LINE-4]] '''42''' Builtin.Int64{{$}}

  var a2 = Int(42)
// CHECK: VarDecl '''a2''' Int{{$}}
// CHECK:         IntegerLiteralExpr:[[@LINE-2]] '''42''' Int64{{$}}
// FULL:  VarDecl '''a2''' swift.Int{{$}}
// FULL:          IntegerLiteralExpr:[[@LINE-4]] '''42''' Builtin.Int64{{$}}

  var a3 = Int16(42)
// CHECK: VarDecl '''a3''' Int16{{$}}
// CHECK:         IntegerLiteralExpr:[[@LINE-2]] '''42''' Int64{{$}}
// FULL:  VarDecl '''a3''' swift.Int16{{$}}
// FULL:          IntegerLiteralExpr:[[@LINE-4]] '''42''' Builtin.Int64{{$}}

  var a4 = Int32(42)
// CHECK: VarDecl '''a4''' Int32{{$}}
// CHECK:         IntegerLiteralExpr:[[@LINE-2]] '''42''' Int64{{$}}
// FULL:  VarDecl '''a4''' swift.Int32{{$}}
// FULL:          IntegerLiteralExpr:[[@LINE-4]] '''42''' Builtin.Int64{{$}}

  var a5 = Int64(42)
// CHECK: VarDecl '''a5''' Int{{$}}
// CHECK:         IntegerLiteralExpr:[[@LINE-2]] '''42''' Int64{{$}}
// FULL:  VarDecl '''a5''' swift.Int{{$}}
// FULL:          IntegerLiteralExpr:[[@LINE-4]] '''42''' Builtin.Int64{{$}}

  var typealias1 = MyInt(42)
// CHECK: VarDecl '''typealias1''' Int{{$}}
// CHECK:         IntegerLiteralExpr:[[@LINE-2]] '''42''' Int64{{$}}
// FULL:  VarDecl '''typealias1''' swift.Int{{$}}
// FULL:          IntegerLiteralExpr:[[@LINE-4]] '''42''' Builtin.Int64{{$}}

  var slice1 = new Int[42]
// CHECK: VarDecl '''slice1''' Int[]{{$}}
// CHECK:   NewArrayExpr:[[@LINE-2]] '''new Int[42]''' Int[]{{$}}
// FULL:  VarDecl '''slice1''' swift.Int[]{{$}}
// FULL:    NewArrayExpr:[[@LINE-4]] '''new Int[42]''' swift.Int[]{{$}}

  var slice2 = new Int[42][2]
// CHECK: VarDecl '''slice2''' (Int[2])[]{{$}}
// CHECK:   NewArrayExpr:[[@LINE-2]] '''new Int[42][2]''' (Int[2])[]{{$}}
// FULL:  VarDecl '''slice2''' (swift.Int[2])[]{{$}}
// FULL:    NewArrayExpr:[[@LINE-4]] '''new Int[42][2]''' (swift.Int[2])[]{{$}}

  var slice3 = new Int[42][2][3]
// CHECK: VarDecl '''slice3''' ((Int[3])[2])[]{{$}}
// CHECK:   NewArrayExpr:[[@LINE-2]] '''new Int[42][2][3]''' ((Int[3])[2])[]{{$}}
// FULL:  VarDecl '''slice3''' ((swift.Int[3])[2])[]{{$}}
// FULL:    NewArrayExpr:[[@LINE-4]] '''new Int[42][2][3]''' ((swift.Int[3])[2])[]{{$}}

  var optional1 = Optional<Int>.None
// CHECK: VarDecl '''optional1''' Optional<Int>{{$}}
// FULL:  VarDecl '''optional1''' swift.Optional<swift.Int>{{$}}

  var optional2 = Optional<Int[]>.None
// CHECK: VarDecl '''optional2''' Optional<Int[]>{{$}}
// FULL:  VarDecl '''optional2''' swift.Optional<swift.Int[]>{{$}}
}

func testFuncType1() {}
// CHECK: FuncDecl '''testFuncType1''' () -> (){{$}}
// FULL:  FuncDecl '''testFuncType1''' () -> (){{$}}

func testFuncType2() -> () {}
// CHECK: FuncDecl '''testFuncType2''' () -> (){{$}}
// FULL:  FuncDecl '''testFuncType2''' () -> (){{$}}

func testFuncType3() -> Void {}
// CHECK: FuncDecl '''testFuncType3''' () -> Void{{$}}
// FULL:  FuncDecl '''testFuncType3''' () -> Void{{$}}

func testFuncType4() -> MyInt {}
// CHECK: FuncDecl '''testFuncType4''' () -> MyInt{{$}}
// FULL:  FuncDecl '''testFuncType4''' () -> MyInt{{$}}

func testFuncType5() -> (Int) {}
// CHECK: FuncDecl '''testFuncType5''' () -> (Int){{$}}
// FULL:  FuncDecl '''testFuncType5''' () -> (Int){{$}}

func testVariadicFuncType(a : Int, b : Float...) {}
// CHECK: FuncDecl '''testVariadicFuncType''' (a : Int, b : Float...) -> (){{$}}
// FULL:  FuncDecl '''testVariadicFuncType''' (a : Int, b : Float...) -> (){{$}}

func testCurriedFuncType1(a : Int)(b : Float) {}
// CHECK: FuncDecl '''testCurriedFuncType1''' (a : Int) -> (b : Float) -> (){{$}}
// FULL:  FuncDecl '''testCurriedFuncType1''' (a : Int) -> (b : Float) -> (){{$}}

func testSelectorFuncType1(a : Int) withFloat(b : Float) {}
// CHECK: FuncDecl '''testSelectorFuncType1''' (Int, withFloat : Float) -> (){{$}}
// FULL:  FuncDecl '''testSelectorFuncType1''' (Int, withFloat : Float) -> (){{$}}

protocol FooProtocol {}
protocol BarProtocol {}
protocol QuxProtocol { typealias Qux }

struct GenericStruct<A, B : FooProtocol> {}

func testInGenericFunc1<A, B : FooProtocol, C : protocol<FooProtocol, BarProtocol>>(a : A, b : B, c : C) {
// CHECK: FuncDecl '''testInGenericFunc1''' <A, B : FooProtocol, C : protocol<BarProtocol, FooProtocol>> (a : A, b : B, c : C) -> (){{$}}
// FULL:  FuncDecl '''testInGenericFunc1''' <A, B : FooProtocol, C : protocol<BarProtocol, FooProtocol>> (a : A, b : B, c : C) -> (){{$}}

  var a1 = a
// CHECK: VarDecl '''a1''' A{{$}}
// FULL:  VarDecl '''a1''' A{{$}}

  var b1 = b
// CHECK: VarDecl '''b1''' B{{$}}
// FULL:  VarDecl '''b1''' B{{$}}

  var gs1 = GenericStruct<A, B>()
// CHECK: VarDecl '''gs1''' GenericStruct<A, B>{{$}}
// CHECK:    CallExpr:[[@LINE-2]] '''GenericStruct<A, B>()''' GenericStruct<A, B>{{$}}
// CHECK:          DeclRefExpr:[[@LINE-3]] '''GenericStruct''' <A, B : FooProtocol> GenericStruct<A, B>.metatype -> () -> GenericStruct<A, B>{{$}}

// FULL:  VarDecl '''gs1''' swift_ide_test.GenericStruct<A, B>{{$}}
// FULL:    CallExpr:[[@LINE-6]] '''GenericStruct<A, B>()''' swift_ide_test.GenericStruct<A, B>{{$}}
// FULL:          DeclRefExpr:[[@LINE-7]] '''GenericStruct''' <A, B : FooProtocol> swift_ide_test.GenericStruct<A, B>.metatype -> () -> GenericStruct<A, B>{{$}}
}

