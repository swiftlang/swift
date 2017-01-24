// RUN: %target-swift-frontend -parse-stdlib -emit-silgen -verify -primary-file %s %S/Inputs/errors_other.swift | %FileCheck %s

import Swift

class Cat {}

enum HomeworkError : Error {
  case TooHard
  case TooMuch
  case CatAteIt(Cat)
}

func someValidPointer<T>() -> UnsafePointer<T> { fatalError() }
func someValidPointer<T>() -> UnsafeMutablePointer<T> { fatalError() }

// CHECK: sil hidden @_TF6errors10make_a_cat{{.*}} : $@convention(thin) () -> (@owned Cat, @error Error) {
// CHECK:      [[T0:%.*]] = function_ref @_TFC6errors3CatC{{.*}} : $@convention(method) (@thick Cat.Type) -> @owned Cat
// CHECK-NEXT: [[T1:%.*]] = metatype $@thick Cat.Type 
// CHECK-NEXT: [[T2:%.*]] = apply [[T0]]([[T1]])
// CHECK-NEXT: return [[T2]] : $Cat
func make_a_cat() throws -> Cat {
  return Cat()
}

// CHECK: sil hidden @_TF6errors15dont_make_a_cat{{.*}} : $@convention(thin) () -> (@owned Cat, @error Error) {
// CHECK:      [[BOX:%.*]] = alloc_existential_box $Error, $HomeworkError
// CHECK-NEXT: [[ADDR:%.*]] = project_existential_box $HomeworkError in [[BOX]] : $Error
// CHECK-NEXT: [[T0:%.*]] = metatype $@thin HomeworkError.Type
// CHECK-NEXT: [[T1:%.*]] = enum $HomeworkError, #HomeworkError.TooHard!enumelt
// CHECK-NEXT: store [[T1]] to [init] [[ADDR]]
// CHECK-NEXT: builtin "willThrow"
// CHECK-NEXT: throw [[BOX]]
func dont_make_a_cat() throws -> Cat {
  throw HomeworkError.TooHard
}

// CHECK: sil hidden @_TF6errors11dont_return{{.*}} : $@convention(thin) <T> (@in T) -> (@out T, @error Error) {
// CHECK:      [[BOX:%.*]] = alloc_existential_box $Error, $HomeworkError
// CHECK-NEXT: [[ADDR:%.*]] = project_existential_box $HomeworkError in [[BOX]] : $Error
// CHECK-NEXT: [[T0:%.*]] = metatype $@thin HomeworkError.Type
// CHECK-NEXT: [[T1:%.*]] = enum $HomeworkError, #HomeworkError.TooMuch!enumelt
// CHECK-NEXT: store [[T1]] to [init] [[ADDR]]
// CHECK-NEXT: builtin "willThrow"
// CHECK-NEXT: destroy_addr %1 : $*T
// CHECK-NEXT: throw [[BOX]]
func dont_return<T>(_ argument: T) throws -> T {
  throw HomeworkError.TooMuch
}

// CHECK:    sil hidden @_TF6errors16all_together_nowFSbCS_3Cat : $@convention(thin) (Bool) -> @owned Cat {
// CHECK:    bb0(%0 : $Bool):
// CHECK:      [[DR_FN:%.*]] = function_ref @_TF6errors11dont_returnur{{.*}} :

//   Branch on the flag.
// CHECK:      cond_br {{%.*}}, [[FLAG_TRUE:bb[0-9]+]], [[FLAG_FALSE:bb[0-9]+]]

//   In the true case, call make_a_cat().
// CHECK:    [[FLAG_TRUE]]:
// CHECK:      [[MAC_FN:%.*]] = function_ref @_TF6errors10make_a_catFzT_CS_3Cat : $@convention(thin) () -> (@owned Cat, @error Error)
// CHECK-NEXT: try_apply [[MAC_FN]]() : $@convention(thin) () -> (@owned Cat, @error Error), normal [[MAC_NORMAL:bb[0-9]+]], error [[MAC_ERROR:bb[0-9]+]]
// CHECK:    [[MAC_NORMAL]]([[T0:%.*]] : $Cat):
// CHECK-NEXT: br [[TERNARY_CONT:bb[0-9]+]]([[T0]] : $Cat)

//   In the false case, call dont_make_a_cat().
// CHECK:    [[FLAG_FALSE]]:
// CHECK:      [[DMAC_FN:%.*]] = function_ref @_TF6errors15dont_make_a_catFzT_CS_3Cat : $@convention(thin) () -> (@owned Cat, @error Error)
// CHECK-NEXT: try_apply [[DMAC_FN]]() : $@convention(thin) () -> (@owned Cat, @error Error), normal [[DMAC_NORMAL:bb[0-9]+]], error [[DMAC_ERROR:bb[0-9]+]]
// CHECK:    [[DMAC_NORMAL]]([[T0:%.*]] : $Cat):
// CHECK-NEXT: br [[TERNARY_CONT]]([[T0]] : $Cat)

//   Merge point for the ternary operator.  Call dont_return with the result.
// CHECK:    [[TERNARY_CONT]]([[T0:%.*]] : $Cat):
// CHECK-NEXT: [[ARG_TEMP:%.*]] = alloc_stack $Cat
// CHECK-NEXT: store [[T0]] to [init] [[ARG_TEMP]]
// CHECK-NEXT: [[RET_TEMP:%.*]] = alloc_stack $Cat
// CHECK-NEXT: try_apply [[DR_FN]]<Cat>([[RET_TEMP]], [[ARG_TEMP]]) : $@convention(thin) <τ_0_0> (@in τ_0_0) -> (@out τ_0_0, @error Error), normal [[DR_NORMAL:bb[0-9]+]], error [[DR_ERROR:bb[0-9]+]]
// CHECK:    [[DR_NORMAL]]({{%.*}} : $()):
// CHECK-NEXT: [[T0:%.*]] = load [take] [[RET_TEMP]] : $*Cat
// CHECK-NEXT: dealloc_stack [[RET_TEMP]]
// CHECK-NEXT: dealloc_stack [[ARG_TEMP]]
// CHECK-NEXT: br [[RETURN:bb[0-9]+]]([[T0]] : $Cat)

//   Return block.
// CHECK:    [[RETURN]]([[T0:%.*]] : $Cat):
// CHECK-NEXT: return [[T0]] : $Cat

//   Catch dispatch block.
// CHECK:    [[CATCH:bb[0-9]+]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT: [[SRC_TEMP:%.*]] = alloc_stack $Error
// CHECK-NEXT: store [[ERROR]] to [init] [[SRC_TEMP]]
// CHECK-NEXT: [[DEST_TEMP:%.*]] = alloc_stack $HomeworkError
// CHECK-NEXT: checked_cast_addr_br copy_on_success Error in [[SRC_TEMP]] : $*Error to HomeworkError in [[DEST_TEMP]] : $*HomeworkError, [[IS_HWE:bb[0-9]+]], [[NOT_HWE:bb[0-9]+]]

//   Catch HomeworkError.
// CHECK:    [[IS_HWE]]:
// CHECK-NEXT: [[T0:%.*]] = load [take] [[DEST_TEMP]] : $*HomeworkError
// CHECK-NEXT: switch_enum [[T0]] : $HomeworkError, case #HomeworkError.CatAteIt!enumelt.1: [[MATCH:bb[0-9]+]], default [[NO_MATCH:bb[0-9]+]]

//   Catch HomeworkError.CatAteIt.
// CHECK:    [[MATCH]]([[T0:%.*]] : $Cat):
// CHECK-NEXT: debug_value
// CHECK-NEXT: [[BORROWED_T0:%.*]] = begin_borrow [[T0]]
// CHECK-NEXT: [[T0_COPY:%.*]] = copy_value [[BORROWED_T0]]
// CHECK-NEXT: end_borrow [[BORROWED_T0]] from [[T0]]
// CHECK-NEXT: destroy_value [[T0]]
// CHECK-NEXT: dealloc_stack [[DEST_TEMP]]
// CHECK-NEXT: destroy_addr [[SRC_TEMP]]
// CHECK-NEXT: dealloc_stack [[SRC_TEMP]]
// CHECK-NEXT: br [[RETURN]]([[T0_COPY]] : $Cat)

//   Catch other HomeworkErrors.
// CHECK:    [[NO_MATCH]]:
// CHECK-NEXT: dealloc_stack [[DEST_TEMP]]
// CHECK-NEXT: dealloc_stack [[SRC_TEMP]]
// CHECK-NEXT: br [[CATCHALL:bb[0-9]+]]

//   Catch other types.
// CHECK:    [[NOT_HWE]]:
// CHECK-NEXT: dealloc_stack [[DEST_TEMP]]
// CHECK-NEXT: dealloc_stack [[SRC_TEMP]]
// CHECK-NEXT: br [[CATCHALL:bb[0-9]+]]

//   Catch all.
// CHECK:    [[CATCHALL]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFC6errors3CatC{{.*}} : $@convention(method) (@thick Cat.Type) -> @owned Cat
// CHECK-NEXT: [[T1:%.*]] = metatype $@thick Cat.Type
// CHECK-NEXT: [[T2:%.*]] = apply [[T0]]([[T1]])
// CHECK-NEXT: destroy_value [[ERROR]] : $Error
// CHECK-NEXT: br [[RETURN]]([[T2]] : $Cat)

//   Landing pad.
// CHECK:    [[MAC_ERROR]]([[T0:%.*]] : $Error):
// CHECK-NEXT: br [[CATCH]]([[T0]] : $Error)
// CHECK:    [[DMAC_ERROR]]([[T0:%.*]] : $Error):
// CHECK-NEXT: br [[CATCH]]([[T0]] : $Error)
// CHECK:    [[DR_ERROR]]([[T0:%.*]] : $Error):
// CHECK-NEXT: dealloc_stack
// CHECK-NEXT: dealloc_stack
// CHECK-NEXT: br [[CATCH]]([[T0]] : $Error)
func all_together_now(_ flag: Bool) -> Cat {
  do {
    return try dont_return(flag ? make_a_cat() : dont_make_a_cat())
  } catch HomeworkError.CatAteIt(let cat) {
    return cat
  } catch _ {
    return Cat()
  }
}

//   Catch in non-throwing context.
// CHECK-LABEL: sil hidden @_TF6errors11catch_a_catFT_CS_3Cat : $@convention(thin) () -> @owned Cat
// CHECK-NEXT: bb0:
// CHECK:      [[F:%.*]] = function_ref @_TFC6errors3CatC{{.*}} : $@convention(method) (@thick Cat.Type) -> @owned Cat
// CHECK-NEXT: [[M:%.*]] = metatype $@thick Cat.Type
// CHECK-NEXT: [[V:%.*]] = apply [[F]]([[M]])
// CHECK-NEXT: return [[V]] : $Cat
func catch_a_cat() -> Cat {
  do {
    return Cat()
  } catch _ as HomeworkError {}  // expected-warning {{'catch' block is unreachable because no errors are thrown in 'do' block}}
}

// Initializers.
class HasThrowingInit {
  var field: Int
  init(value: Int) throws {
    field = value
  }
}
// CHECK-LABEL: sil hidden @_TFC6errors15HasThrowingInitc{{.*}} : $@convention(method) (Int, @owned HasThrowingInit) -> (@owned HasThrowingInit, @error Error) {
// CHECK:      [[T0:%.*]] = mark_uninitialized [rootself] %1 : $HasThrowingInit
// CHECK-NEXT: [[BORROWED_T0:%.*]] = begin_borrow [[T0]]
// CHECK-NEXT: [[T1:%.*]] = ref_element_addr [[BORROWED_T0]] : $HasThrowingInit
// CHECK-NEXT: assign %0 to [[T1]] : $*Int
// CHECK-NEXT: end_borrow [[BORROWED_T0]] from [[T0]]
// CHECK-NEXT: [[T0_RET:%.*]] = copy_value [[T0]]
// CHECK-NEXT: destroy_value [[T0]]
// CHECK-NEXT: return [[T0_RET]] : $HasThrowingInit

// CHECK-LABEL: sil hidden @_TFC6errors15HasThrowingInitC{{.*}} : $@convention(method) (Int, @thick HasThrowingInit.Type) -> (@owned HasThrowingInit, @error Error)
// CHECK:      [[SELF:%.*]] = alloc_ref $HasThrowingInit
// CHECK:      [[T0:%.*]] = function_ref @_TFC6errors15HasThrowingInitc{{.*}} : $@convention(method) (Int, @owned HasThrowingInit) -> (@owned HasThrowingInit, @error Error)
// CHECK-NEXT: try_apply [[T0]](%0, [[SELF]]) : $@convention(method) (Int, @owned HasThrowingInit) -> (@owned HasThrowingInit, @error Error), normal bb1, error bb2
// CHECK:    bb1([[SELF:%.*]] : $HasThrowingInit):
// CHECK-NEXT: return [[SELF]]
// CHECK:    bb2([[ERROR:%.*]] : $Error):
// CHECK-NEXT: builtin "willThrow"
// CHECK-NEXT: throw [[ERROR]]


enum ColorError : Error {
  case Red, Green, Blue
}

//CHECK-LABEL: sil hidden @_TF6errors6IThrowFzT_Vs5Int32
//CHECK: builtin "willThrow"
//CHECK-NEXT: throw
func IThrow() throws -> Int32 {
  throw ColorError.Red
  return 0  // expected-warning {{will never be executed}}
}

// Make sure that we are not emitting calls to 'willThrow' on rethrow sites.
//CHECK-LABEL: sil hidden @_TF6errors12DoesNotThrowFzT_Vs5Int32
//CHECK-NOT: builtin "willThrow"
//CHECK: return
func DoesNotThrow() throws -> Int32 {
  _ = try IThrow()
  return 2
}

// rdar://20782111
protocol Doomed {
  func check() throws
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV6errors12DoomedStructS_6DoomedS_FS1_5check{{.*}} : $@convention(witness_method) (@in_guaranteed DoomedStruct) -> @error Error
// CHECK:      [[TEMP:%.*]] = alloc_stack $DoomedStruct
// CHECK:      copy_addr %0 to [initialization] [[TEMP]]
// CHECK:      [[SELF:%.*]] = load [trivial] [[TEMP]] : $*DoomedStruct
// CHECK:      [[T0:%.*]] = function_ref @_TFV6errors12DoomedStruct5check{{.*}} : $@convention(method) (DoomedStruct) -> @error Error
// CHECK-NEXT: try_apply [[T0]]([[SELF]])
// CHECK:    bb1([[T0:%.*]] : $()):
// CHECK:      [[T0:%.*]] = tuple ()
// CHECK:      dealloc_stack [[TEMP]]
// CHECK:      return [[T0]] : $()
// CHECK:    bb2([[T0:%.*]] : $Error):
// CHECK:      builtin "willThrow"([[T0]] : $Error)
// CHECK:      dealloc_stack [[TEMP]]
// CHECK:      throw [[T0]] : $Error
struct DoomedStruct : Doomed {
  func check() throws {}
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC6errors11DoomedClassS_6DoomedS_FS1_5check{{.*}} : $@convention(witness_method) (@in_guaranteed DoomedClass) -> @error Error {
// CHECK:      [[TEMP:%.*]] = alloc_stack $DoomedClass
// CHECK:      copy_addr %0 to [initialization] [[TEMP]]
// CHECK:      [[SELF:%.*]] = load [take] [[TEMP]] : $*DoomedClass
// CHECK:      [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
// CHECK:      [[T0:%.*]] = class_method [[BORROWED_SELF]] : $DoomedClass, #DoomedClass.check!1 : (DoomedClass) -> () throws -> (), $@convention(method) (@guaranteed DoomedClass) -> @error Error
// CHECK-NEXT: try_apply [[T0]]([[BORROWED_SELF]])
// CHECK:    bb1([[T0:%.*]] : $()):
// CHECK:      [[T0:%.*]] = tuple ()
// CHECK:      end_borrow [[BORROWED_SELF]] from [[SELF]]
// CHECK:      destroy_value [[SELF]] : $DoomedClass
// CHECK:      dealloc_stack [[TEMP]]
// CHECK:      return [[T0]] : $()
// CHECK:    bb2([[T0:%.*]] : $Error):
// CHECK:      builtin "willThrow"([[T0]] : $Error)
// CHECK:      end_borrow [[BORROWED_SELF]] from [[SELF]]
// CHECK:      destroy_value [[SELF]] : $DoomedClass
// CHECK:      dealloc_stack [[TEMP]]
// CHECK:      throw [[T0]] : $Error
class DoomedClass : Doomed {
  func check() throws {}
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV6errors11HappyStructS_6DoomedS_FS1_5check{{.*}} : $@convention(witness_method) (@in_guaranteed HappyStruct) -> @error Error
// CHECK:      [[TEMP:%.*]] = alloc_stack $HappyStruct
// CHECK:      copy_addr %0 to [initialization] [[TEMP]]
// CHECK:      [[SELF:%.*]] = load [trivial] [[TEMP]] : $*HappyStruct
// CHECK:      [[T0:%.*]] = function_ref @_TFV6errors11HappyStruct5check{{.*}} : $@convention(method) (HappyStruct) -> ()
// CHECK:      [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:      [[T1:%.*]] = tuple ()
// CHECK:      dealloc_stack [[TEMP]]
// CHECK:      return [[T1]] : $()
struct HappyStruct : Doomed {
  func check() {}
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC6errors10HappyClassS_6DoomedS_FS1_5check{{.*}} : $@convention(witness_method) (@in_guaranteed HappyClass) -> @error Error
// CHECK:      [[TEMP:%.*]] = alloc_stack $HappyClass
// CHECK:      copy_addr %0 to [initialization] [[TEMP]]
// CHECK:      [[SELF:%.*]] = load [take] [[TEMP]] : $*HappyClass
// CHECK:      [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
// CHECK:      [[T0:%.*]] = class_method [[BORROWED_SELF]] : $HappyClass, #HappyClass.check!1 : (HappyClass) -> () -> (), $@convention(method) (@guaranteed HappyClass) -> ()
// CHECK:      [[T1:%.*]] = apply [[T0]]([[BORROWED_SELF]])
// CHECK:      [[T1:%.*]] = tuple ()
// CHECK:      end_borrow [[BORROWED_SELF]] from [[SELF]]
// CHECK:      destroy_value [[SELF]] : $HappyClass
// CHECK:      dealloc_stack [[TEMP]]
// CHECK:      return [[T1]] : $()
class HappyClass : Doomed {
  func check() {}
}

func create<T>(_ fn: () throws -> T) throws -> T {
  return try fn()
}
func testThunk(_ fn: () throws -> Int) throws -> Int {
  return try create(fn)
}
// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo__dSizoPs5Error__XFo__iSizoPS___ : $@convention(thin) (@owned @callee_owned () -> (Int, @error Error)) -> (@out Int, @error Error)
// CHECK: bb0(%0 : $*Int, %1 : $@callee_owned () -> (Int, @error Error)):
// CHECK:   try_apply %1()
// CHECK: bb1([[T0:%.*]] : $Int):
// CHECK:   store [[T0]] to [trivial] %0 : $*Int
// CHECK:   [[T0:%.*]] = tuple ()
// CHECK:   return [[T0]]
// CHECK: bb2([[T0:%.*]] : $Error):
// CHECK:   builtin "willThrow"([[T0]] : $Error)
// CHECK:   throw [[T0]] : $Error

func createInt(_ fn: () -> Int) throws {}
func testForceTry(_ fn: () -> Int) {
  try! createInt(fn)
}
// CHECK-LABEL: sil hidden @_TF6errors12testForceTryFFT_SiT_ : $@convention(thin) (@owned @callee_owned () -> Int) -> ()
// CHECK: bb0([[ARG:%.*]] : $@callee_owned () -> Int):
// CHECK: [[FUNC:%.*]] = function_ref @_TF6errors9createIntFzFT_SiT_ : $@convention(thin) (@owned @callee_owned () -> Int) -> @error Error
// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK: try_apply [[FUNC]]([[ARG_COPY]])
// CHECK: end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK: destroy_value [[ARG]]
// CHECK: return
// CHECK: builtin "unexpectedError"
// CHECK: unreachable

func testForceTryMultiple() {
  _ = try! (make_a_cat(), make_a_cat())
}

// CHECK-LABEL: sil hidden @_TF6errors20testForceTryMultipleFT_T_
// CHECK-NEXT: bb0:
// CHECK: [[FN_1:%.+]] = function_ref @_TF6errors10make_a_catFzT_CS_3Cat
// CHECK-NEXT: try_apply [[FN_1]]() : $@convention(thin) () -> (@owned Cat, @error Error), normal [[SUCCESS_1:[^ ]+]], error [[CLEANUPS_1:[^ ]+]],
// CHECK: [[SUCCESS_1]]([[VALUE_1:%.+]] : $Cat)
// CHECK: [[FN_2:%.+]] = function_ref @_TF6errors10make_a_catFzT_CS_3Cat
// CHECK-NEXT: try_apply [[FN_2]]() : $@convention(thin) () -> (@owned Cat, @error Error), normal [[SUCCESS_2:[^ ]+]], error [[CLEANUPS_2:[^ ]+]],
// CHECK: [[SUCCESS_2]]([[VALUE_2:%.+]] : $Cat)
// CHECK-NEXT: destroy_value [[VALUE_2]] : $Cat
// CHECK-NEXT: destroy_value [[VALUE_1]] : $Cat
// CHECK-NEXT: [[VOID:%.+]] = tuple ()
// CHECK-NEXT: return [[VOID]] : $()
// CHECK: [[FAILURE:.+]]([[ERROR:%.+]] : $Error):
// CHECK-NEXT: = builtin "unexpectedError"([[ERROR]] : $Error)
// CHECK-NEXT: unreachable
// CHECK: [[CLEANUPS_1]]([[ERROR:%.+]] : $Error):
// CHECK-NEXT: br [[FAILURE]]([[ERROR]] : $Error)
// CHECK: [[CLEANUPS_2]]([[ERROR:%.+]] : $Error):
// CHECK-NEXT: destroy_value [[VALUE_1]] : $Cat
// CHECK-NEXT: br [[FAILURE]]([[ERROR]] : $Error)
// CHECK: } // end sil function '_TF6errors20testForceTryMultipleFT_T_'

// Make sure we balance scopes correctly inside a switch.
// <rdar://problem/20923654>
enum CatFood {
  case Canned
  case Dry
}

// Something we can switch on that throws.
func preferredFood() throws -> CatFood {
  return CatFood.Canned
}

func feedCat() throws -> Int {
  switch try preferredFood() {
  case .Canned:
    return 0
  case .Dry:
    return 1
  }
}
// CHECK-LABEL: sil hidden @_TF6errors7feedCatFzT_Si : $@convention(thin) () -> (Int, @error Error)
// CHECK: debug_value undef : $Error, var, name "$error", argno 1
// CHECK:   %1 = function_ref @_TF6errors13preferredFoodFzT_OS_7CatFood : $@convention(thin) () -> (CatFood, @error Error)
// CHECK:   try_apply %1() : $@convention(thin) () -> (CatFood, @error Error), normal bb1, error bb5
// CHECK: bb1([[VAL:%.*]] : $CatFood):
// CHECK:   switch_enum [[VAL]] : $CatFood, case #CatFood.Canned!enumelt: bb2, case #CatFood.Dry!enumelt: bb3
// CHECK: bb5([[ERROR:%.*]] : $Error)
// CHECK:   throw [[ERROR]] : $Error

// Throwing statements inside cases.
func getHungryCat(_ food: CatFood) throws -> Cat {
  switch food {
  case .Canned:
    return try make_a_cat()
  case .Dry:
    return try dont_make_a_cat()
  }
}
// errors.getHungryCat throws (errors.CatFood) -> errors.Cat
// CHECK-LABEL: sil hidden @_TF6errors12getHungryCatFzOS_7CatFoodCS_3Cat : $@convention(thin) (CatFood) -> (@owned Cat, @error Error)
// CHECK: bb0(%0 : $CatFood):
// CHECK:   debug_value undef : $Error, var, name "$error", argno 2
// CHECK:   switch_enum %0 : $CatFood, case #CatFood.Canned!enumelt: bb1, case #CatFood.Dry!enumelt: bb3
// CHECK: bb1:
// CHECK:   [[FN:%.*]] = function_ref @_TF6errors10make_a_catFzT_CS_3Cat : $@convention(thin) () -> (@owned Cat, @error Error)
// CHECK:   try_apply [[FN]]() : $@convention(thin) () -> (@owned Cat, @error Error), normal bb2, error bb6
// CHECK: bb3:
// CHECK:   [[FN:%.*]] = function_ref @_TF6errors15dont_make_a_catFzT_CS_3Cat : $@convention(thin) () -> (@owned Cat, @error Error)
// CHECK:   try_apply [[FN]]() : $@convention(thin) () -> (@owned Cat, @error Error), normal bb4, error bb7
// CHECK: bb6([[ERROR:%.*]] : $Error):
// CHECK:   br bb8([[ERROR:%.*]] : $Error)
// CHECK: bb7([[ERROR:%.*]] : $Error):
// CHECK:   br bb8([[ERROR]] : $Error)
// CHECK: bb8([[ERROR:%.*]] : $Error):
// CHECK:   throw [[ERROR]] : $Error

func take_many_cats(_ cats: Cat...) throws {}
func test_variadic(_ cat: Cat) throws {
  try take_many_cats(make_a_cat(), cat, make_a_cat(), make_a_cat())
}

// CHECK-LABEL: sil hidden @_TF6errors13test_variadicFzCS_3CatT_ : $@convention(thin) (@owned Cat) -> @error Error {
// CHECK:       bb0([[ARG:%.*]] : $Cat):
// CHECK:         debug_value undef : $Error, var, name "$error", argno 2
// CHECK:         [[TAKE_FN:%.*]] = function_ref @_TF6errors14take_many_catsFztGSaCS_3Cat__T_ : $@convention(thin) (@owned Array<Cat>) -> @error Error
// CHECK:         [[N:%.*]] = integer_literal $Builtin.Word, 4
// CHECK:         [[T0:%.*]] = function_ref @_TFs27_allocateUninitializedArray
// CHECK:         [[T1:%.*]] = apply [[T0]]<Cat>([[N]])
// CHECK:         [[BORROWED_T1:%.*]] = begin_borrow [[T1]]
// CHECK:         [[BORROWED_ARRAY:%.*]] = tuple_extract [[BORROWED_T1]] :  $(Array<Cat>, Builtin.RawPointer), 0
// CHECK:         [[ARRAY:%.*]] = copy_value [[BORROWED_ARRAY]]
// CHECK:         [[T2:%.*]] = tuple_extract [[BORROWED_T1]] :  $(Array<Cat>, Builtin.RawPointer), 1
// CHECK:         end_borrow [[BORROWED_T1]] from [[T1]]
// CHECK:         destroy_value [[T1]]
// CHECK:         [[ELT0:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*Cat
//   Element 0.
// CHECK:         [[T0:%.*]] = function_ref @_TF6errors10make_a_catFzT_CS_3Cat : $@convention(thin) () -> (@owned Cat, @error Error)
// CHECK:         try_apply [[T0]]() : $@convention(thin) () -> (@owned Cat, @error Error), normal [[NORM_0:bb[0-9]+]], error [[ERR_0:bb[0-9]+]]
// CHECK:       [[NORM_0]]([[CAT0:%.*]] : $Cat):
// CHECK-NEXT:    store [[CAT0]] to [init] [[ELT0]]
//   Element 1.
// CHECK-NEXT:    [[T0:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[ELT1:%.*]] = index_addr [[ELT0]] : $*Cat, [[T0]]
// CHECK-NEXT:    [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK-NEXT:    [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK-NEXT:    store [[ARG_COPY]] to [init] [[ELT1]]
//   Element 2.
// CHECK-NEXT:    [[T0:%.*]] = integer_literal $Builtin.Word, 2
// CHECK-NEXT:    [[ELT2:%.*]] = index_addr [[ELT0]] : $*Cat, [[T0]]
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[T0:%.*]] = function_ref @_TF6errors10make_a_catFzT_CS_3Cat : $@convention(thin) () -> (@owned Cat, @error Error)
// CHECK-NEXT:    try_apply [[T0]]() : $@convention(thin) () -> (@owned Cat, @error Error), normal [[NORM_2:bb[0-9]+]], error [[ERR_2:bb[0-9]+]]
// CHECK:       [[NORM_2]]([[CAT2:%.*]] : $Cat):
// CHECK-NEXT:    store [[CAT2]] to [init] [[ELT2]]
//   Element 3.
// CHECK-NEXT:    [[T0:%.*]] = integer_literal $Builtin.Word, 3
// CHECK-NEXT:    [[ELT3:%.*]] = index_addr [[ELT0]] : $*Cat, [[T0]]
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[T0:%.*]] = function_ref @_TF6errors10make_a_catFzT_CS_3Cat : $@convention(thin) () -> (@owned Cat, @error Error)
// CHECK-NEXT:    try_apply [[T0]]() : $@convention(thin) () -> (@owned Cat, @error Error), normal [[NORM_3:bb[0-9]+]], error [[ERR_3:bb[0-9]+]]
// CHECK:       [[NORM_3]]([[CAT3:%.*]] : $Cat):
// CHECK-NEXT:    store [[CAT3]] to [init] [[ELT3]]
//   Complete the call and return.
// CHECK-NEXT:    try_apply [[TAKE_FN]]([[ARRAY]]) : $@convention(thin) (@owned Array<Cat>) -> @error Error, normal [[NORM_CALL:bb[0-9]+]], error [[ERR_CALL:bb[0-9]+]]
// CHECK:       [[NORM_CALL]]([[T0:%.*]] : $()):
// CHECK-NEXT:    end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK-NEXT:    destroy_value [[ARG]] : $Cat
// CHECK-NEXT:    [[T0:%.*]] = tuple ()
// CHECK-NEXT:    return
//   Failure from element 0.
// CHECK:       [[ERR_0]]([[ERROR:%.*]] : $Error):
// CHECK-NOT:     end_borrow
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[T0:%.*]] = function_ref @_TFs29_deallocateUninitializedArray
// CHECK-NEXT:    apply [[T0]]<Cat>([[ARRAY]])
// CHECK-NEXT:    br [[RETHROW:.*]]([[ERROR]] : $Error)
//   Failure from element 2.
// CHECK:       [[ERR_2]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT:    end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK-NEXT:    destroy_addr [[ELT1]]
// CHECK-NEXT:    destroy_addr [[ELT0]]
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[T0:%.*]] = function_ref @_TFs29_deallocateUninitializedArray
// CHECK-NEXT:    apply [[T0]]<Cat>([[ARRAY]])
// CHECK-NEXT:    br [[RETHROW]]([[ERROR]] : $Error)
//   Failure from element 3.
// CHECK:       [[ERR_3]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT:    destroy_addr [[ELT2]]
// CHECK-NEXT:    end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK-NEXT:    destroy_addr [[ELT1]]
// CHECK-NEXT:    destroy_addr [[ELT0]]
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[T0:%.*]] = function_ref @_TFs29_deallocateUninitializedArray
// CHECK-NEXT:    apply [[T0]]<Cat>([[ARRAY]])
// CHECK-NEXT:    br [[RETHROW]]([[ERROR]] : $Error)
//   Failure from call.
// CHECK:       [[ERR_CALL]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT:    end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK-NEXT:    br [[RETHROW]]([[ERROR]] : $Error)
//   Rethrow.
// CHECK:       [[RETHROW]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT:    destroy_value [[ARG]] : $Cat
// CHECK-NEXT:    throw [[ERROR]]
// CHECK: } // end sil function '_TF6errors13test_variadicFzCS_3CatT_'

// rdar://20861374
// Clear out the self box before delegating.
class BaseThrowingInit : HasThrowingInit {
  var subField: Int
  init(value: Int, subField: Int) throws {
    self.subField = subField
    try super.init(value: value)
  }
}
// CHECK: sil hidden @_TFC6errors16BaseThrowingInitc{{.*}} : $@convention(method) (Int, Int, @owned BaseThrowingInit) -> (@owned BaseThrowingInit, @error Error)
// CHECK:      [[BOX:%.*]] = alloc_box ${ var BaseThrowingInit }
// CHECK:      [[PB:%.*]] = project_box [[BOX]]
// CHECK:      [[MARKED_BOX:%.*]] = mark_uninitialized [derivedself] [[PB]]
//   Initialize subField.
// CHECK:      [[T0:%.*]] = load_borrow [[MARKED_BOX]]
// CHECK-NEXT: [[T1:%.*]] = ref_element_addr [[T0]] : $BaseThrowingInit, #BaseThrowingInit.subField
// CHECK-NEXT: assign %1 to [[T1]]
//   Super delegation.
// CHECK-NEXT: [[T0:%.*]] = load_borrow [[MARKED_BOX]]
// CHECK-NEXT: [[T2:%.*]] = upcast [[T0]] : $BaseThrowingInit to $HasThrowingInit
// CHECK: [[T3:%[0-9]+]] = function_ref @_TFC6errors15HasThrowingInitcfzT5valueSi_S0_ : $@convention(method) (Int, @owned HasThrowingInit) -> (@owned HasThrowingInit, @error Error)
// CHECK-NEXT: apply [[T3]](%0, [[T2]])

// Cleanups for writebacks.

protocol Supportable {
  mutating func support() throws
}
protocol Buildable {
  associatedtype Structure : Supportable
  var firstStructure: Structure { get set }
  subscript(name: String) -> Structure { get set }
}
func supportFirstStructure<B: Buildable>(_ b: inout B) throws {
  try b.firstStructure.support()
}
// CHECK-LABEL: sil hidden @_TF6errors21supportFirstStructure{{.*}} : $@convention(thin) <B where B : Buildable> (@inout B) -> @error Error {
// CHECK: [[SUPPORT:%.*]] = witness_method $B.Structure, #Supportable.support!1 :
// CHECK: [[MATBUFFER:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
// CHECK: [[BUFFER:%.*]] = alloc_stack $B.Structure
// CHECK: [[BUFFER_CAST:%.*]] = address_to_pointer [[BUFFER]] : $*B.Structure to $Builtin.RawPointer
// CHECK: [[MAT:%.*]] = witness_method $B, #Buildable.firstStructure!materializeForSet.1 :
// CHECK: [[T1:%.*]] = apply [[MAT]]<B>([[BUFFER_CAST]], [[MATBUFFER]], [[BASE:%[0-9]*]])
// CHECK: [[T2:%.*]] = tuple_extract [[T1]] : {{.*}}, 0
// CHECK: [[CALLBACK:%.*]] = tuple_extract [[T1]] : {{.*}}, 1
// CHECK: [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*B.Structure
// CHECK: [[T4:%.*]] = mark_dependence [[T3]] : $*B.Structure on [[BASE]] : $*B
// CHECK: try_apply [[SUPPORT]]<B.Structure>([[T4]]) : {{.*}}, normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERROR:bb[0-9]+]]

// CHECK: [[BB_NORMAL]]
// CHECK: switch_enum [[CALLBACK]]
// CHECK: apply
// CHECK: dealloc_stack [[BUFFER]]
// CHECK: dealloc_stack [[MATBUFFER]]
// CHECK: return

// CHECK: [[BB_ERROR]]([[ERROR:%.*]] : $Error):
// CHECK: switch_enum [[CALLBACK]]
// CHECK: apply
// CHECK: dealloc_stack [[BUFFER]]
// CHECK: dealloc_stack [[MATBUFFER]]
// CHECK: throw [[ERROR]]

func supportStructure<B: Buildable>(_ b: inout B, name: String) throws {
  try b[name].support()
}
// CHECK-LABEL: sil hidden @_TF6errors16supportStructure
// CHECK: bb0({{.*}}, [[INDEX:%.*]] : $String):
// CHECK:   [[SUPPORT:%.*]] = witness_method $B.Structure, #Supportable.support!1 :
// CHECK:   [[BORROWED_INDEX:%.*]] = begin_borrow [[INDEX]]
// CHECK:   [[INDEX_COPY:%.*]] = copy_value [[BORROWED_INDEX]] : $String
// CHECK:   [[MATBUFFER:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
// CHECK:   [[BUFFER:%.*]] = alloc_stack $B.Structure
// CHECK:   [[BUFFER_CAST:%.*]] = address_to_pointer [[BUFFER]] : $*B.Structure to $Builtin.RawPointer
// CHECK:   [[MAT:%.*]] = witness_method $B, #Buildable.subscript!materializeForSet.1 :
// CHECK:   [[T1:%.*]] = apply [[MAT]]<B>([[BUFFER_CAST]], [[MATBUFFER]], [[INDEX_COPY]], [[BASE:%[0-9]*]])
// CHECK:   [[T2:%.*]] = tuple_extract [[T1]] : {{.*}}, 0
// CHECK:   [[CALLBACK:%.*]] = tuple_extract [[T1]] : {{.*}}, 1
// CHECK:   [[T3:%.*]] = pointer_to_address [[T2]] : $Builtin.RawPointer to [strict] $*B.Structure
// CHECK:   [[T4:%.*]] = mark_dependence [[T3]] : $*B.Structure on [[BASE]] : $*B
// CHECK:   try_apply [[SUPPORT]]<B.Structure>([[T4]]) : {{.*}}, normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERROR:bb[0-9]+]]

// CHECK: [[BB_NORMAL]]
// CHECK:   switch_enum [[CALLBACK]]
// CHECK:   apply
// CHECK:   dealloc_stack [[BUFFER]]
// CHECK:   dealloc_stack [[MATBUFFER]]
// CHECK:   end_borrow [[BORROWED_INDEX]] from [[INDEX]]
// CHECK:   destroy_value [[INDEX]] : $String
// CHECK:   return

// CHECK: [[BB_ERROR]]([[ERROR:%.*]] : $Error):
// CHECK:   switch_enum [[CALLBACK]]

// CHECK:   apply
// CHECK:   dealloc_stack [[BUFFER]]
// CHECK:   dealloc_stack [[MATBUFFER]]
// CHECK:   destroy_value [[INDEX]] : $String
// CHECK:   throw [[ERROR]]

struct Pylon {
  var name: String
  mutating func support() throws {}
}
struct Bridge {
  var mainPylon : Pylon
  subscript(name: String) -> Pylon {
    get {
      return mainPylon
    }
    set {}
  }
}
func supportStructure(_ b: inout Bridge, name: String) throws {
  try b[name].support()
}
// CHECK:    sil hidden @_TF6errors16supportStructureFzTRVS_6Bridge4nameSS_T_ : $@convention(thin) (@inout Bridge, @owned String) -> @error Error {
// CHECK:    bb0([[ARG1:%.*]] : $*Bridge, [[ARG2:%.*]] : $String):
// CHECK:      [[SUPPORT:%.*]] = function_ref @_TFV6errors5Pylon7support
// CHECK-NEXT: [[BORROWED_ARG2:%.*]] = begin_borrow [[ARG2]]
// CHECK-NEXT: [[INDEX_COPY_1:%.*]] = copy_value [[BORROWED_ARG2]] : $String
// CHECK-NEXT: [[INDEX_COPY_2:%.*]] = copy_value [[INDEX_COPY_1]] : $String
// CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $Pylon
// CHECK-NEXT: [[BASE:%.*]] = load [copy] [[ARG1]] : $*Bridge
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[GETTER:%.*]] = function_ref @_TFV6errors6Bridgeg9subscriptFSSVS_5Pylon :
// CHECK-NEXT: [[T0:%.*]] = apply [[GETTER]]([[INDEX_COPY_1]], [[BASE]])
// CHECK-NEXT: destroy_value [[BASE]]
// CHECK-NEXT: store [[T0]] to [init] [[TEMP]]
// CHECK-NEXT: try_apply [[SUPPORT]]([[TEMP]]) : {{.*}}, normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERROR:bb[0-9]+]]

// CHECK:    [[BB_NORMAL]]
// CHECK-NEXT: [[T0:%.*]] = load [take] [[TEMP]]
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[SETTER:%.*]] = function_ref @_TFV6errors6Bridges9subscriptFSSVS_5Pylon :
// CHECK-NEXT: apply [[SETTER]]([[T0]], [[INDEX_COPY_2]], [[ARG1]])
// CHECK-NEXT: dealloc_stack [[TEMP]]
// CHECK-NEXT: end_borrow [[BORROWED_INDEX]] from [[INDEX]]
// CHECK-NEXT: destroy_value [[INDEX]] : $String
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return

//   We end up with ugly redundancy here because we don't want to
//   consume things during cleanup emission.  It's questionable.
// CHECK:    [[BB_ERROR]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT: [[T0:%.*]] = load [copy] [[TEMP]]
// CHECK-NEXT: [[INDEX_COPY_2_COPY:%.*]] = copy_value [[INDEX_COPY_2]]
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[SETTER:%.*]] = function_ref @_TFV6errors6Bridges9subscriptFSSVS_5Pylon :
// CHECK-NEXT: apply [[SETTER]]([[T0]], [[INDEX_COPY_2_COPY]], [[ARG1]])
// CHECK-NEXT: destroy_addr [[TEMP]]
// CHECK-NEXT: dealloc_stack [[TEMP]]
// ==> SEMANTIC ARC TODO: INDEX_COPY_2 on the next line should be INDEX_COPY_2_COPY
// CHECK-NEXT: destroy_value [[INDEX_COPY_2]] : $String
// CHECK-NEXT: end_borrow [[BORROWED_INDEX]] from [[INDEX]]
// CHECK-NEXT: destroy_value [[INDEX]] : $String
// CHECK-NEXT: throw [[ERROR]]
// CHECK: } // end sil function '_TF6errors16supportStructureFzTRVS_6Bridge4nameSS_T_'

struct OwnedBridge {
  var owner : Builtin.UnknownObject
  subscript(name: String) -> Pylon {
    addressWithOwner { return (someValidPointer(), owner) }
    mutableAddressWithOwner { return (someValidPointer(), owner) }
  }
}
func supportStructure(_ b: inout OwnedBridge, name: String) throws {
  try b[name].support()
}
// CHECK:    sil hidden @_TF6errors16supportStructureFzTRVS_11OwnedBridge4nameSS_T_ :
// CHECK:    bb0([[ARG1:%.*]] : $*OwnedBridge, [[ARG2:%.*]] : $String):
// CHECK:      [[SUPPORT:%.*]] = function_ref @_TFV6errors5Pylon7support
// CHECK:      [[BORROWED_ARG2:%.*]] = begin_borrow [[ARG2]]
// CHECK:      [[ARG2_COPY:%.*]] = copy_value [[BORROWED_ARG2]] : $String
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[ADDRESSOR:%.*]] = function_ref @_TFV6errors11OwnedBridgeaO9subscriptFSSVS_5Pylon :
// CHECK-NEXT: [[T0:%.*]] = apply [[ADDRESSOR]]([[ARG2_COPY]], [[ARG1]])
// CHECK-NEXT: [[BORROWED_T0:%.*]] = begin_borrow [[T0]]
// CHECK-NEXT: [[T1:%.*]] = tuple_extract [[BORROWED_T0]] : {{.*}}, 0
// CHECK-NEXT: [[BORROWED_OWNER:%.*]] = tuple_extract [[BORROWED_T0]] : {{.*}}, 1
// CHECK-NEXT: [[OWNER:%.*]] = copy_value [[BORROWED_OWNER]]
// CHECK-NEXT: end_borrow [[BORROWED_T0]] from [[T0]]
// CHECK-NEXT: destroy_value [[T0]]
// CHECK-NEXT: [[T3:%.*]] = struct_extract [[T1]]
// CHECK-NEXT: [[T4:%.*]] = pointer_to_address [[T3]]
// CHECK-NEXT: [[T5:%.*]] = mark_dependence [[T4]] : $*Pylon on [[OWNER]]
// CHECK-NEXT: try_apply [[SUPPORT]]([[T5]]) : {{.*}}, normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERROR:bb[0-9]+]]
// CHECK:    [[BB_NORMAL]]
// CHECK-NEXT: destroy_value [[OWNER]] : $Builtin.UnknownObject
// CHECK-NEXT: end_borrow [[BORROWED_ARG2]] from [[ARG2]]
// CHECK-NEXT: destroy_value [[ARG2]] : $String
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return
// CHECK:    [[BB_ERROR]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT: destroy_value [[OWNER]] : $Builtin.UnknownObject
// CHECK-NEXT: end_borrow [[BORROWED_ARG2]] from [[ARG2]]
// CHECK-NEXT: destroy_value [[ARG2]] : $String
// CHECK-NEXT: throw [[ERROR]]
// CHECK: } // end sil function '_TF6errors16supportStructureFzTRVS_11OwnedBridge4nameSS_T_'

struct PinnedBridge {
  var owner : Builtin.NativeObject
  subscript(name: String) -> Pylon {
    addressWithPinnedNativeOwner { return (someValidPointer(), owner) }
    mutableAddressWithPinnedNativeOwner { return (someValidPointer(), owner) }
  }
}
func supportStructure(_ b: inout PinnedBridge, name: String) throws {
  try b[name].support()
}
// CHECK:      sil hidden @_TF6errors16supportStructureFzTRVS_12PinnedBridge4nameSS_T_ :
// CHECK:      bb0([[ARG1:%.*]] : $*PinnedBridge, [[ARG2:%.*]] : $String):
// CHECK:        [[SUPPORT:%.*]] = function_ref @_TFV6errors5Pylon7support
// CHECK:        [[BORROWED_ARG2:%.*]] = begin_borrow [[ARG2]]
// CHECK:        [[ARG2_COPY:%.*]] = copy_value [[BORROWED_ARG2]] : $String
// CHECK-NEXT:   // function_ref
// CHECK-NEXT:   [[ADDRESSOR:%.*]] = function_ref @_TFV6errors12PinnedBridgeap9subscriptFSSVS_5Pylon :
// CHECK-NEXT:   [[T0:%.*]] = apply [[ADDRESSOR]]([[ARG2_COPY]], [[ARG1]])
// CHECK-NEXT:   [[BORROWED_T0:%.*]] = begin_borrow [[T0]]
// CHECK-NEXT:   [[T1:%.*]] = tuple_extract [[BORROWED_T0]] : {{.*}}, 0
// CHECK-NEXT:   [[BORROWED_OWNER:%.*]] = tuple_extract [[BORROWED_T0]] : {{.*}}, 1
// CHECK-NEXT:   [[OWNER:%.*]] = copy_value [[BORROWED_OWNER]]
// CHECK-NEXT:   end_borrow [[BORROWED_T0]] from [[T0]]
// CHECK-NEXT:   destroy_value [[T0]]
// CHECK-NEXT:   [[T3:%.*]] = struct_extract [[T1]]
// CHECK-NEXT:   [[T4:%.*]] = pointer_to_address [[T3]]
// CHECK-NEXT:   [[T5:%.*]] = mark_dependence [[T4]] : $*Pylon on [[OWNER]]
// CHECK-NEXT:   try_apply [[SUPPORT]]([[T5]]) : {{.*}}, normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERROR:bb[0-9]+]]
// CHECK:      [[BB_NORMAL]]
// CHECK-NEXT:   strong_unpin [[OWNER]] : $Optional<Builtin.NativeObject>
// CHECK-NEXT:   end_borrow [[BORROWED_ARG2]] from [[ARG2]]
// CHECK-NEXT:   destroy_value [[ARG2]] : $String
// CHECK-NEXT:   tuple ()
// CHECK-NEXT:   return
// CHECK:      [[BB_ERROR]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT:   [[OWNER_COPY:%.*]] = copy_value [[OWNER]]
// CHECK-NEXT:   strong_unpin [[OWNER_COPY]] : $Optional<Builtin.NativeObject>
// CHECK-NEXT:   destroy_value [[OWNER]]
// CHECK-NEXT:   end_borrow [[BORROWED_ARG2]] from [[ARG2]]
// CHECK-NEXT:   destroy_value [[ARG2]] : $String
// CHECK-NEXT:   throw [[ERROR]]
// CHECK: } // end sil function '_TF6errors16supportStructureFzTRVS_12PinnedBridge4nameSS_T_'

// ! peepholes its argument with getSemanticsProvidingExpr().
// Test that that doesn't look through try!.
// rdar://21515402
func testForcePeephole(_ f: () throws -> Int?) -> Int {
  let x = (try! f())!
  return x
}

// CHECK-LABEL: sil hidden @_TF6errors15testOptionalTryFT_T_
// CHECK-NEXT: bb0:
// CHECK: [[FN:%.+]] = function_ref @_TF6errors10make_a_catFzT_CS_3Cat
// CHECK-NEXT: try_apply [[FN]]() : $@convention(thin) () -> (@owned Cat, @error Error), normal [[SUCCESS:[^ ]+]], error [[CLEANUPS:[^ ]+]],
// CHECK: [[SUCCESS]]([[VALUE:%.+]] : $Cat)
// CHECK-NEXT: [[WRAPPED:%.+]] = enum $Optional<Cat>, #Optional.some!enumelt.1, [[VALUE]]
// CHECK-NEXT: br [[DONE:[^ ]+]]([[WRAPPED]] : $Optional<Cat>)
// CHECK: [[DONE]]([[RESULT:%.+]] : $Optional<Cat>):
// CHECK-NEXT: destroy_value [[RESULT]] : $Optional<Cat>
// CHECK-NEXT: [[VOID:%.+]] = tuple ()
// CHECK-NEXT: return [[VOID]] : $()
// CHECK: [[FAILURE:.+]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT: destroy_value [[ERROR]]
// CHECK-NEXT: [[NONE:%.+]] = enum $Optional<Cat>, #Optional.none!enumelt
// CHECK-NEXT: br [[DONE]]([[NONE]] : $Optional<Cat>)
// CHECK: [[CLEANUPS]]([[ERROR:%.+]] : $Error):
// CHECK-NEXT: br [[FAILURE]]([[ERROR]] : $Error)
// CHECK: } // end sil function '_TF6errors15testOptionalTryFT_T_'
func testOptionalTry() {
  _ = try? make_a_cat()
}

// CHECK-LABEL: sil hidden @_TF6errors18testOptionalTryVarFT_T_
// CHECK-NEXT: bb0:
// CHECK-NEXT: [[BOX:%.+]] = alloc_box ${ var Optional<Cat> }
// CHECK-NEXT: [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT: [[BOX_DATA:%.+]] = init_enum_data_addr [[PB]] : $*Optional<Cat>, #Optional.some!enumelt.1
// CHECK: [[FN:%.+]] = function_ref @_TF6errors10make_a_catFzT_CS_3Cat
// CHECK-NEXT: try_apply [[FN]]() : $@convention(thin) () -> (@owned Cat, @error Error), normal [[SUCCESS:[^ ]+]], error [[CLEANUPS:[^ ]+]],
// CHECK: [[SUCCESS]]([[VALUE:%.+]] : $Cat)
// CHECK-NEXT: store [[VALUE]] to [init] [[BOX_DATA]] : $*Cat
// CHECK-NEXT: inject_enum_addr [[PB]] : $*Optional<Cat>, #Optional.some!enumelt.1
// CHECK-NEXT: br [[DONE:[^ ]+]],
// CHECK: [[DONE]]:
// CHECK-NEXT: destroy_value [[BOX]] : ${ var Optional<Cat> }
// CHECK-NEXT: [[VOID:%.+]] = tuple ()
// CHECK-NEXT: return [[VOID]] : $()
// CHECK: [[FAILURE:.+]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT: destroy_value [[ERROR]]
// CHECK-NEXT: inject_enum_addr [[PB]] : $*Optional<Cat>, #Optional.none!enumelt
// CHECK-NEXT: br [[DONE]]
// CHECK: [[CLEANUPS]]([[ERROR:%.+]] : $Error):
// CHECK-NEXT: br [[FAILURE]]([[ERROR]] : $Error)
// CHECK: } // end sil function '_TF6errors18testOptionalTryVarFT_T_'
func testOptionalTryVar() {
  var cat = try? make_a_cat() // expected-warning {{initialization of variable 'cat' was never used; consider replacing with assignment to '_' or removing it}}
}

// CHECK-LABEL: sil hidden @_TF6errors26testOptionalTryAddressOnly
// CHECK: bb0(%0 : $*T):
// CHECK: [[BOX:%.+]] = alloc_stack $Optional<T>
// CHECK-NEXT: [[BOX_DATA:%.+]] = init_enum_data_addr [[BOX]] : $*Optional<T>, #Optional.some!enumelt.1
// CHECK: [[FN:%.+]] = function_ref @_TF6errors11dont_return
// CHECK-NEXT: [[ARG_BOX:%.+]] = alloc_stack $T
// CHECK-NEXT: copy_addr %0 to [initialization] [[ARG_BOX]] : $*T
// CHECK-NEXT: try_apply [[FN]]<T>([[BOX_DATA]], [[ARG_BOX]]) : $@convention(thin) <τ_0_0> (@in τ_0_0) -> (@out τ_0_0, @error Error), normal [[SUCCESS:[^ ]+]], error [[CLEANUPS:[^ ]+]],
// CHECK: [[SUCCESS]]({{%.+}} : $()):
// CHECK-NEXT: inject_enum_addr [[BOX]] : $*Optional<T>, #Optional.some!enumelt.1
// CHECK-NEXT: dealloc_stack [[ARG_BOX]] : $*T
// CHECK-NEXT: br [[DONE:[^ ]+]],
// CHECK: [[DONE]]:
// CHECK-NEXT: destroy_addr [[BOX]] : $*Optional<T>
// CHECK-NEXT: dealloc_stack [[BOX]] : $*Optional<T>
// CHECK-NEXT: destroy_addr %0 : $*T
// CHECK-NEXT: [[VOID:%.+]] = tuple ()
// CHECK-NEXT: return [[VOID]] : $()
// CHECK: [[FAILURE:.+]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT: destroy_value [[ERROR]]
// CHECK-NEXT: inject_enum_addr [[BOX]] : $*Optional<T>, #Optional.none!enumelt
// CHECK-NEXT: br [[DONE]]
// CHECK: [[CLEANUPS]]([[ERROR:%.+]] : $Error):
// CHECK-NEXT: dealloc_stack [[ARG_BOX]] : $*T
// CHECK-NEXT: br [[FAILURE]]([[ERROR]] : $Error)
// CHECK: } // end sil function '_TF6errors26testOptionalTryAddressOnlyurFxT_'
func testOptionalTryAddressOnly<T>(_ obj: T) {
  _ = try? dont_return(obj)
}

// CHECK-LABEL: sil hidden @_TF6errors29testOptionalTryAddressOnlyVar
// CHECK: bb0(%0 : $*T):
// CHECK: [[BOX:%.+]] = alloc_box $<τ_0_0> { var Optional<τ_0_0> } <T>
// CHECK-NEXT: [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT: [[BOX_DATA:%.+]] = init_enum_data_addr [[PB]] : $*Optional<T>, #Optional.some!enumelt.1
// CHECK: [[FN:%.+]] = function_ref @_TF6errors11dont_return
// CHECK-NEXT: [[ARG_BOX:%.+]] = alloc_stack $T
// CHECK-NEXT: copy_addr %0 to [initialization] [[ARG_BOX]] : $*T
// CHECK-NEXT: try_apply [[FN]]<T>([[BOX_DATA]], [[ARG_BOX]]) : $@convention(thin) <τ_0_0> (@in τ_0_0) -> (@out τ_0_0, @error Error), normal [[SUCCESS:[^ ]+]], error [[CLEANUPS:[^ ]+]],
// CHECK: [[SUCCESS]]({{%.+}} : $()):
// CHECK-NEXT: inject_enum_addr [[PB]] : $*Optional<T>, #Optional.some!enumelt.1
// CHECK-NEXT: dealloc_stack [[ARG_BOX]] : $*T
// CHECK-NEXT: br [[DONE:[^ ]+]],
// CHECK: [[DONE]]:
// CHECK-NEXT: destroy_value [[BOX]] : $<τ_0_0> { var Optional<τ_0_0> } <T>
// CHECK-NEXT: destroy_addr %0 : $*T
// CHECK-NEXT: [[VOID:%.+]] = tuple ()
// CHECK-NEXT: return [[VOID]] : $()
// CHECK: [[FAILURE:.+]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT: destroy_value [[ERROR]]
// CHECK-NEXT: inject_enum_addr [[PB]] : $*Optional<T>, #Optional.none!enumelt
// CHECK-NEXT: br [[DONE]]
// CHECK: [[CLEANUPS]]([[ERROR:%.+]] : $Error):
// CHECK-NEXT: dealloc_stack [[ARG_BOX]] : $*T
// CHECK-NEXT: br [[FAILURE]]([[ERROR]] : $Error)
// CHECK: } // end sil function '_TF6errors29testOptionalTryAddressOnlyVarurFxT_'
func testOptionalTryAddressOnlyVar<T>(_ obj: T) {
  var copy = try? dont_return(obj) // expected-warning {{initialization of variable 'copy' was never used; consider replacing with assignment to '_' or removing it}}
}

// CHECK-LABEL: sil hidden @_TF6errors23testOptionalTryMultipleFT_T_
// CHECK: bb0:
// CHECK: [[FN_1:%.+]] = function_ref @_TF6errors10make_a_catFzT_CS_3Cat
// CHECK-NEXT: try_apply [[FN_1]]() : $@convention(thin) () -> (@owned Cat, @error Error), normal [[SUCCESS_1:[^ ]+]], error [[CLEANUPS_1:[^ ]+]],
// CHECK: [[SUCCESS_1]]([[VALUE_1:%.+]] : $Cat)
// CHECK: [[FN_2:%.+]] = function_ref @_TF6errors10make_a_catFzT_CS_3Cat
// CHECK-NEXT: try_apply [[FN_2]]() : $@convention(thin) () -> (@owned Cat, @error Error), normal [[SUCCESS_2:[^ ]+]], error [[CLEANUPS_2:[^ ]+]],
// CHECK: [[SUCCESS_2]]([[VALUE_2:%.+]] : $Cat)
// CHECK-NEXT: [[TUPLE:%.+]] = tuple ([[VALUE_1]] : $Cat, [[VALUE_2]] : $Cat)
// CHECK-NEXT: [[WRAPPED:%.+]] = enum $Optional<(Cat, Cat)>, #Optional.some!enumelt.1, [[TUPLE]]
// CHECK-NEXT: br [[DONE:[^ ]+]]([[WRAPPED]] : $Optional<(Cat, Cat)>)
// CHECK: [[DONE]]([[RESULT:%.+]] : $Optional<(Cat, Cat)>):
// CHECK-NEXT: destroy_value [[RESULT]] : $Optional<(Cat, Cat)>
// CHECK-NEXT: [[VOID:%.+]] = tuple ()
// CHECK-NEXT: return [[VOID]] : $()
// CHECK: [[FAILURE:.+]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT: destroy_value [[ERROR]]
// CHECK-NEXT: [[NONE:%.+]] = enum $Optional<(Cat, Cat)>, #Optional.none!enumelt
// CHECK-NEXT: br [[DONE]]([[NONE]] : $Optional<(Cat, Cat)>)
// CHECK: [[CLEANUPS_1]]([[ERROR:%.+]] : $Error):
// CHECK-NEXT: br [[FAILURE]]([[ERROR]] : $Error)
// CHECK: [[CLEANUPS_2]]([[ERROR:%.+]] : $Error):
// CHECK-NEXT: destroy_value [[VALUE_1]] : $Cat
// CHECK-NEXT: br [[FAILURE]]([[ERROR]] : $Error)
// CHECK: } // end sil function '_TF6errors23testOptionalTryMultipleFT_T_'
func testOptionalTryMultiple() {
  _ = try? (make_a_cat(), make_a_cat())
}

// CHECK-LABEL: sil hidden @_TF6errors25testOptionalTryNeverFailsFT_T_
// CHECK: bb0:
// CHECK-NEXT:   [[VALUE:%.+]] = tuple ()
// CHECK-NEXT:   = enum $Optional<()>, #Optional.some!enumelt.1, [[VALUE]]
// CHECK-NEXT:   [[VOID:%.+]] = tuple ()
// CHECK-NEXT:   return [[VOID]] : $()
// CHECK: } // end sil function '_TF6errors25testOptionalTryNeverFailsFT_T_'
func testOptionalTryNeverFails() {
  _ = try? () // expected-warning {{no calls to throwing functions occur within 'try' expression}}
}

// CHECK-LABEL: sil hidden @_TF6errors28testOptionalTryNeverFailsVarFT_T_
// CHECK: bb0:
// CHECK-NEXT:   [[BOX:%.+]] = alloc_box ${ var Optional<()> }
// CHECK-NEXT:   [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:   = init_enum_data_addr [[PB]] : $*Optional<()>, #Optional.some!enumelt.1
// CHECK-NEXT:   inject_enum_addr [[PB]] : $*Optional<()>, #Optional.some!enumelt.1
// CHECK-NEXT:   destroy_value [[BOX]] : ${ var Optional<()> }
// CHECK-NEXT:   [[VOID:%.+]] = tuple ()
// CHECK-NEXT:   return [[VOID]] : $()
// CHECK-NEXT: } // end sil function '_TF6errors28testOptionalTryNeverFailsVarFT_T_'
func testOptionalTryNeverFailsVar() {
  var unit: ()? = try? () // expected-warning {{no calls to throwing functions occur within 'try' expression}} expected-warning {{variable 'unit' was never used; consider replacing with '_' or removing it}}
}

// CHECK-LABEL: sil hidden @_TF6errors36testOptionalTryNeverFailsAddressOnly
// CHECK: bb0(%0 : $*T):
// CHECK:   [[BOX:%.+]] = alloc_stack $Optional<T>
// CHECK-NEXT:   [[BOX_DATA:%.+]] = init_enum_data_addr [[BOX]] : $*Optional<T>, #Optional.some!enumelt.1
// CHECK-NEXT:   copy_addr %0 to [initialization] [[BOX_DATA]] : $*T
// CHECK-NEXT:   inject_enum_addr [[BOX]] : $*Optional<T>, #Optional.some!enumelt.1
// CHECK-NEXT:   destroy_addr [[BOX]] : $*Optional<T>
// CHECK-NEXT:   dealloc_stack [[BOX]] : $*Optional<T>
// CHECK-NEXT:   destroy_addr %0 : $*T
// CHECK-NEXT:   [[VOID:%.+]] = tuple ()
// CHECK-NEXT:   return [[VOID]] : $()
// CHECK-NEXT: } // end sil function '_TF6errors36testOptionalTryNeverFailsAddressOnlyurFxT_'
func testOptionalTryNeverFailsAddressOnly<T>(_ obj: T) {
  _ = try? obj // expected-warning {{no calls to throwing functions occur within 'try' expression}}
}

// CHECK-LABEL: sil hidden @_TF6errors39testOptionalTryNeverFailsAddressOnlyVar
// CHECK: bb0(%0 : $*T):
// CHECK:   [[BOX:%.+]] = alloc_box $<τ_0_0> { var Optional<τ_0_0> } <T>
// CHECK-NEXT:   [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:   [[BOX_DATA:%.+]] = init_enum_data_addr [[PB]] : $*Optional<T>, #Optional.some!enumelt.1
// CHECK-NEXT:   copy_addr %0 to [initialization] [[BOX_DATA]] : $*T
// CHECK-NEXT:   inject_enum_addr [[PB]] : $*Optional<T>, #Optional.some!enumelt.1
// CHECK-NEXT:   destroy_value [[BOX]] : $<τ_0_0> { var Optional<τ_0_0> } <T>
// CHECK-NEXT:   destroy_addr %0 : $*T
// CHECK-NEXT:   [[VOID:%.+]] = tuple ()
// CHECK-NEXT:   return [[VOID]] : $()
// CHECK: } // end sil function '_TFC6errors13OtherErrorSubCfT_S0_'
func testOptionalTryNeverFailsAddressOnlyVar<T>(_ obj: T) {
  var copy = try? obj // expected-warning {{no calls to throwing functions occur within 'try' expression}} expected-warning {{initialization of variable 'copy' was never used; consider replacing with assignment to '_' or removing it}}
}

class SomeErrorClass : Error { }

// CHECK-LABEL: sil_vtable SomeErrorClass
// CHECK-NEXT:   #SomeErrorClass.deinit!deallocator: _TFC6errors14SomeErrorClassD
// CHECK-NEXT:   #SomeErrorClass.init!initializer.1: {{.*}} : _TFC6errors14SomeErrorClasscfT_S0_
// CHECK-NEXT: }

class OtherErrorSub : OtherError { }

// CHECK-LABEL: sil_vtable OtherErrorSub {
// CHECK-NEXT:  #OtherError.init!initializer.1: {{.*}} : _TFC6errors13OtherErrorSubcfT_S0_     // OtherErrorSub.init() -> OtherErrorSub
// CHECK-NEXT:  #OtherErrorSub.deinit!deallocator: _TFC6errors13OtherErrorSubD        // OtherErrorSub.__deallocating_deinit
// CHECK-NEXT:}
