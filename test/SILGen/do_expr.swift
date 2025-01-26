// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature ThenStatements -enable-experimental-feature DoExpressions %s | %FileCheck %s
// RUN: %target-swift-emit-ir -enable-experimental-feature ThenStatements -enable-experimental-feature DoExpressions %s

// REQUIRES: swift_feature_DoExpressions
// REQUIRES: swift_feature_ThenStatements

@discardableResult
func throwsError(_ x: Int = 0) throws -> Int { 0 }

struct Err: Error {}

func test1() -> Int {
  do { 5 }
}
// CHECK-LABEL: sil hidden [ossa] @$s7do_expr5test1SiyF : $@convention(thin) () -> Int
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       store {{%[0-9]+}} to [trivial] [[RESULT]] : $*Int
// CHECK-NEXT:  [[RET:%[0-9]+]] = load [trivial] [[RESULT]] : $*Int
// CHECK-NEXT:  dealloc_stack [[RESULT]] : $*Int
// CHECK-NEXT:  return [[RET]] : $Int

func test2() -> Int {
  return do { 5 }
}

func test3() -> Int {
  let x = do { 5 }
  return x
}

func test4() -> Int {
  do { then 5 }
}

func test5() -> Int {
  let x = do { (); then 5 }
  return x
}

func test6() -> Int {
  let x = do {
    let y = 0
    try throwsError()
    then y
  } catch {
    7
  }
  return x
}
// CHECK-LABEL: sil hidden [ossa] @$s7do_expr5test6SiyF : $@convention(thin) () -> Int
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       [[Y_LIT:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 0
// CHECK:       [[Y:%[0-9]+]] = apply {{%[0-9]+}}([[Y_LIT]], {{%[0-9]+}})
// CHECK:       [[MVY:%.*]] = move_value [var_decl] [[Y]] : $Int
// CHECK:       [[THROWS_ERR_FN:%[0-9]+]] = function_ref @$s7do_expr11throwsErroryS2iKF : $@convention(thin) (Int) -> (Int, @error any Error)
// CHECK:       try_apply [[THROWS_ERR_FN]]({{%[0-9]+}}) : $@convention(thin) (Int) -> (Int, @error any Error), normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERR:bb[0-9]+]]
//
// CHECK:       [[BB_NORMAL]]
// CHECK-NEXT:  ignored_use
// CHECK-NEXT:  store [[MVY]] to [trivial] [[RESULT]] : $*Int
// CHECK-NEXT:  extend_lifetime [[MVY]] : $Int
// CHECK-NEXT:  br [[BB_EXIT:bb[0-9]+]]
//
// CHECK:       [[BB_EXIT]]
// CHECK:       [[RET:%[0-9]+]] = load [trivial] [[RESULT]] : $*Int
// CHECK:       [[MVR:%[0-9]+]] = move_value [var_decl] [[RET]] : $Int
// CHECK:       dealloc_stack [[RESULT]] : $*Int
// CHECK:       return [[MVR]] : $Int
//
// CHECK:       [[BB_ERR]]
// CHECK:       [[SEVEN_LIT:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 7
// CHECK:       [[SEVEN:%[0-9]+]] = apply {{%[0-9]+}}([[SEVEN_LIT]], {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:       store [[SEVEN]] to [trivial] [[RESULT]] : $*Int
// CHECK:       br [[BB_EXIT]]

func test7() throws -> Int {
  let x = do {
    let y = 0
    then try throwsError(y)
  } catch _ where .random() {
    then try throwsError()
  } catch {
    7
  }
  return x
}
// CHECK-LABEL: sil hidden [ossa] @$s7do_expr5test7SiyKF : $@convention(thin) () -> (Int, @error any Error)
// CHECK:       [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK:       [[Y_LIT:%[0-9]]] = integer_literal $Builtin.IntLiteral, 0
// CHECK:       [[Y:%[0-9]+]] = apply {{%[0-9]+}}([[Y_LIT]], {{%[0-9]+}})
// CHECK:       [[MVY:%.*]] = move_value [var_decl] [[Y]] : $Int
// CHECK:       [[THROWS_ERR_FN:%[0-9]]] = function_ref @$s7do_expr11throwsErroryS2iKF : $@convention(thin) (Int) -> (Int, @error any Error)
// CHECK:       try_apply [[THROWS_ERR_FN]]([[MVY]]) : $@convention(thin) (Int) -> (Int, @error any Error), normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERR:bb[0-9]+]]
//
// CHECK:       [[BB_NORMAL]]([[I:%[0-9]+]] : $Int)
// CHECK-NEXT:  store [[I]] to [trivial] [[RESULT]] : $*Int
// CHECK-NEXT:  extend_lifetime [[MVY]] : $Int
// CHECK-NEXT:  br [[BB_EXIT:bb[0-9]+]]
//
// CHECK:       [[BB_EXIT]]
// CHECK:       [[RET:%[0-9]+]] = load [trivial] [[RESULT]] : $*Int
// CHECK:       [[MVR:%.*]] = move_value [var_decl] [[RET]] : $Int
// CHECK:       dealloc_stack [[RESULT]] : $*Int
// CHECK:       return [[MVR]] : $Int
//
// CHECK:       [[BB_ERR]]
// CHECK:       function_ref @$sSb6randomSbyFZ : $@convention(method) (@thin Bool.Type) -> Bool
// CHECK:       cond_br {{%[0-9]+}}, [[BB_FIRST_CATCH:bb[0-9]+]], [[BB_SECOND_CATCH:bb[0-9]+]]
//
// CHECK:       [[BB_FIRST_CATCH]]
// CHECK:       [[THROWS_ERR_FN:%[0-9]+]] = function_ref @$s7do_expr11throwsErroryS2iKF : $@convention(thin) (Int) -> (Int, @error any Error)
// CHECK:       try_apply [[THROWS_ERR_FN]]({{%[0-9]+}}) : $@convention(thin) (Int) -> (Int, @error any Error), normal [[BB_NORMAL2:bb[0-9]+]], error [[BB_ERR2:bb[0-9]+]]
//
// CHECK:       [[BB_NORMAL2]]([[I:%[0-9]+]] : $Int):
// CHECK-NEXT:  store [[I]] to [trivial] [[RESULT]] : $*Int
// CHECK:       br [[BB_EXIT:bb[0-9]+]]
//
// CHECK:       [[BB_SECOND_CATCH]]
// CHECK:       [[SEVEN_LIT:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 7
// CHECK:       [[SEVEN:%[0-9]+]] = apply {{%[0-9]+}}([[SEVEN_LIT]], {{%[0-9]+}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK:       store [[SEVEN]] to [trivial] [[RESULT]] : $*Int
// CHECK:       br [[BB_EXIT]]
//
// CHECK:       [[BB_ERR2]]([[ERR:%[0-9]+]] : @owned $any Error)
// CHECK:       dealloc_stack [[RESULT]] : $*Int
// CHECK:       throw [[ERR]] : $any Error

func test8() throws -> Int {
  var x = 0
  x = do {
    let y = 0
    then try throwsError(y)
  } catch _ where .random() {
    then try throwsError()
  } catch {
    8
  }
  return x
}

func test9() throws -> Int {
  let fn = {
    do {
      let y = 0
      then try throwsError(y)
    } catch _ where .random() {
      then try throwsError()
    } catch {
      8
    }
  }
  return try fn()
}

func test10() -> Int {
  do { 5 } as Int
}

func testExhaustive1() -> Error {
  // We can syntactically determine that 'let x' is a non-refutable pattern.
  let err = do {
    try throwsError()
    then Err() as Error
  } catch let x {
    x
  }
  return err
}

func testExhaustive2() -> Error {
  do {
    try throwsError()
    then Err() as Error
  } catch let x {
    x
  }
}

func throwAndReturnError() throws -> Error { fatalError() }

func testExhaustive3() -> Error {
  do {
    try throwAndReturnError()
  } catch let x {
    x
  }
}

func testExhaustive4() {
  // We can syntactically determine that '_' is a non-refutable pattern.
  let _ = do { try throwsError() } catch _ { 0 }
}
