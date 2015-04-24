// This file should not have any syntax or type checker errors.
// RUN: %target-parse-verify-swift

// RUN: %target-swift-ide-test -print-types -source-filename %s -fully-qualified-types=false | FileCheck %s -strict-whitespace
// RUN: %target-swift-ide-test -print-types -source-filename %s -fully-qualified-types=true | FileCheck %s -check-prefix=FULL -strict-whitespace

typealias MyInt = Int
// CHECK: TypeAliasDecl '''MyInt''' MyInt.Type{{$}}
// FULL:  TypeAliasDecl '''MyInt''' swift_ide_test.MyInt.Type{{$}}

func testVariableTypes(param: Int, inout param2: Double) {
// CHECK: FuncDecl '''testVariableTypes''' (Int, inout param2: Double) -> (){{$}}
// FULL:  FuncDecl '''testVariableTypes''' (Swift.Int, inout param2: Swift.Double) -> (){{$}}

  var a1 = 42
// CHECK: VarDecl '''a1''' Int{{$}}
// CHECK:         IntegerLiteralExpr:[[@LINE-2]] '''42''' Int2048{{$}}
// FULL:  VarDecl '''a1''' Swift.Int{{$}}
// FULL:          IntegerLiteralExpr:[[@LINE-4]] '''42''' Builtin.Int2048{{$}}

  var a2 : Int = 42
// CHECK: VarDecl '''a2''' Int{{$}}
// CHECK:         IntegerLiteralExpr:[[@LINE-2]] '''42''' Int2048{{$}}
// FULL:  VarDecl '''a2''' Swift.Int{{$}}
// FULL:          IntegerLiteralExpr:[[@LINE-4]] '''42''' Builtin.Int2048{{$}}

  var a3 = Int16(42)
// CHECK: VarDecl '''a3''' Int16{{$}}
// CHECK:         IntegerLiteralExpr:[[@LINE-2]] '''42''' Int2048{{$}}
// FULL:  VarDecl '''a3''' Swift.Int16{{$}}
// FULL:          IntegerLiteralExpr:[[@LINE-4]] '''42''' Builtin.Int2048{{$}}

  var a4 = Int32(42)
// CHECK: VarDecl '''a4''' Int32{{$}}
// CHECK:         IntegerLiteralExpr:[[@LINE-2]] '''42''' Int2048{{$}}
// FULL:  VarDecl '''a4''' Swift.Int32{{$}}
// FULL:          IntegerLiteralExpr:[[@LINE-4]] '''42''' Builtin.Int2048{{$}}

  var a5 : Int64 = 42
// CHECK: VarDecl '''a5''' Int64{{$}}
// CHECK:         IntegerLiteralExpr:[[@LINE-2]] '''42''' Int2048{{$}}
// FULL:  VarDecl '''a5''' Swift.Int64{{$}}
// FULL:          IntegerLiteralExpr:[[@LINE-4]] '''42''' Builtin.Int2048{{$}}

  var typealias1 : MyInt = 42
// CHECK: VarDecl '''typealias1''' MyInt{{$}}
// CHECK:         IntegerLiteralExpr:[[@LINE-2]] '''42''' Int2048{{$}}
// FULL:  VarDecl '''typealias1''' swift_ide_test.MyInt{{$}}
// FULL:          IntegerLiteralExpr:[[@LINE-4]] '''42''' Builtin.Int2048{{$}}

  var optional1 = Optional<Int>.None
// CHECK: VarDecl '''optional1''' Optional<Int>{{$}}
// FULL:  VarDecl '''optional1''' Swift.Optional<Swift.Int>{{$}}

  var optional2 = Optional<[Int]>.None
// CHECK: VarDecl '''optional2''' Optional<[Int]>{{$}}
// FULL:  VarDecl '''optional2''' Swift.Optional<[Swift.Int]>{{$}}
}

func testFuncType1() {}
// CHECK: FuncDecl '''testFuncType1''' () -> (){{$}}
// FULL:  FuncDecl '''testFuncType1''' () -> (){{$}}

func testFuncType2() -> () {}
// CHECK: FuncDecl '''testFuncType2''' () -> (){{$}}
// FULL:  FuncDecl '''testFuncType2''' () -> (){{$}}

func testFuncType3() -> Void {}
// CHECK: FuncDecl '''testFuncType3''' () -> Void{{$}}
// FULL:  FuncDecl '''testFuncType3''' () -> Swift.Void{{$}}

func testFuncType4() -> MyInt {}
// CHECK: FuncDecl '''testFuncType4''' () -> MyInt{{$}}
// FULL:  FuncDecl '''testFuncType4''' () -> swift_ide_test.MyInt{{$}}

func testFuncType5() -> (Int) {}
// CHECK: FuncDecl '''testFuncType5''' () -> (Int){{$}}
// FULL:  FuncDecl '''testFuncType5''' () -> (Swift.Int){{$}}

func testFuncType6() -> (Int, Int) {}
// CHECK: FuncDecl '''testFuncType6''' () -> (Int, Int){{$}}
// FULL:  FuncDecl '''testFuncType6''' () -> (Swift.Int, Swift.Int){{$}}

func testFuncType7(a: Int, withFloat b: Float) {}
// CHECK: FuncDecl '''testFuncType7''' (Int, withFloat: Float) -> (){{$}}
// FULL:  FuncDecl '''testFuncType7''' (Swift.Int, withFloat: Swift.Float) -> (){{$}}

func testVariadicFuncType(a: Int, b: Float...) {}
// CHECK: FuncDecl '''testVariadicFuncType''' (Int, b: Float...) -> (){{$}}
// FULL:  FuncDecl '''testVariadicFuncType''' (Swift.Int, b: Swift.Float...) -> (){{$}}

func testCurriedFuncType1(a: Int)(b: Float) {}
// CHECK: FuncDecl '''testCurriedFuncType1''' (Int) -> (b: Float) -> (){{$}}
// FULL:  FuncDecl '''testCurriedFuncType1''' (Swift.Int) -> (b: Swift.Float) -> (){{$}}

protocol FooProtocol {}
protocol BarProtocol {}
protocol QuxProtocol { typealias Qux }

struct GenericStruct<A, B : FooProtocol> {}

func testInGenericFunc1<A, B : FooProtocol, C : protocol<FooProtocol, BarProtocol>>(a: A, b: B, c: C) {
// CHECK: FuncDecl '''testInGenericFunc1''' <A, B : FooProtocol, C : protocol<FooProtocol, BarProtocol>> (A, b: B, c: C) -> (){{$}}
// FULL:  FuncDecl '''testInGenericFunc1''' <A, B : FooProtocol, C : protocol<FooProtocol, BarProtocol>> (A, b: B, c: C) -> (){{$}}

  var a1 = a
// CHECK: VarDecl '''a1''' A{{$}}
// FULL:  VarDecl '''a1''' A{{$}}

  var b1 = b
// CHECK: VarDecl '''b1''' B{{$}}
// FULL:  VarDecl '''b1''' B{{$}}

  var gs1 = GenericStruct<A, B>()
// CHECK: VarDecl '''gs1''' GenericStruct<A, B>{{$}}
// CHECK:    CallExpr:[[@LINE-2]] '''GenericStruct<A, B>()''' GenericStruct<A, B>{{$}}
// CHECK:          ConstructorRefCallExpr:[[@LINE-3]] '''GenericStruct<A, B>''' () -> GenericStruct<A, B>

// FULL:  VarDecl '''gs1''' swift_ide_test.GenericStruct<A, B>{{$}}
// FULL:    CallExpr:[[@LINE-6]] '''GenericStruct<A, B>()''' swift_ide_test.GenericStruct<A, B>{{$}}
// FULL:          ConstructorRefCallExpr:[[@LINE-7]] '''GenericStruct<A, B>''' () -> swift_ide_test.GenericStruct<A, B>
}

func testInGenericFunc2<T : QuxProtocol, U : QuxProtocol where T.Qux == U.Qux>() {}
// CHECK: FuncDecl '''testInGenericFunc2''' <T : QuxProtocol, U : QuxProtocol where T.Qux == U.Qux> () -> (){{$}}
// FULL:  FuncDecl '''testInGenericFunc2''' <T : QuxProtocol, U : QuxProtocol where T.Qux == U.Qux> () -> (){{$}}

