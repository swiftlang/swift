// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

class Cat {}

enum HomeworkError : _ErrorType {
  case TooHard
  case TooMuch
  case CatAteIt(Cat)
}

// CHECK: sil hidden @_TF6errors10make_a_catFzT_CS_3Cat : $@convention(thin) () -> (@owned Cat, @error _ErrorType) {
// CHECK:      [[T0:%.*]] = function_ref @_TFC6errors3CatCfMS0_FT_S0_ : $@convention(thin) (@thick Cat.Type) -> @owned Cat
// CHECK-NEXT: [[T1:%.*]] = metatype $@thick Cat.Type 
// CHECK-NEXT: [[T2:%.*]] = apply [[T0]]([[T1]])
// CHECK-NEXT: return [[T2]] : $Cat
func make_a_cat() throws -> Cat {
  return Cat()
}

// CHECK: sil hidden @_TF6errors15dont_make_a_catFzT_CS_3Cat : $@convention(thin) () -> (@owned Cat, @error _ErrorType) {
// CHECK:      [[BOX:%.*]] = alloc_existential_box $_ErrorType, $HomeworkError
// CHECK:      [[T0:%.*]] = function_ref @_TFO6errors13HomeworkError7TooHardFMS0_S0_ : $@convention(thin) (@thin HomeworkError.Type) -> @owned HomeworkError
// CHECK-NEXT: [[T1:%.*]] = metatype $@thin HomeworkError.Type
// CHECK-NEXT: [[T2:%.*]] = apply [[T0]]([[T1]])
// CHECK-NEXT: store [[T2]] to [[BOX]]#1
// CHECK-NEXT: builtin "willThrow"
// CHECK-NEXT: throw [[BOX]]#0
func dont_make_a_cat() throws -> Cat {
  throw HomeworkError.TooHard
}

// CHECK: sil hidden @_TF6errors11dont_returnU__FzQ_Q_ : $@convention(thin) <T> (@out T, @in T) -> @error _ErrorType {
// CHECK:      [[BOX:%.*]] = alloc_existential_box $_ErrorType, $HomeworkError
// CHECK:      [[T0:%.*]] = function_ref @_TFO6errors13HomeworkError7TooMuchFMS0_S0_ : $@convention(thin) (@thin HomeworkError.Type) -> @owned HomeworkError
// CHECK-NEXT: [[T1:%.*]] = metatype $@thin HomeworkError.Type
// CHECK-NEXT: [[T2:%.*]] = apply [[T0]]([[T1]])
// CHECK-NEXT: store [[T2]] to [[BOX]]#1
// CHECK-NEXT: builtin "willThrow"
// CHECK-NEXT: destroy_addr %1 : $*T
// CHECK-NEXT: throw [[BOX]]#0
func dont_return<T>(argument: T) throws -> T {
  throw HomeworkError.TooMuch
}

// CHECK:    sil hidden @_TF6errors16all_together_nowFSbCS_3Cat : $@convention(thin) (Bool) -> @owned Cat {
// CHECK:    bb0(%0 : $Bool):
// CHECK:      [[DR_FN:%.*]] = function_ref @_TF6errors11dont_returnU__FzQ_Q_ :

//   Branch on the flag.
// CHECK:      cond_br {{%.*}}, [[FLAG_TRUE:bb[0-9]+]], [[FLAG_FALSE:bb[0-9]+]]

//   In the true case, call make_a_cat().
// CHECK:    [[FLAG_TRUE]]:
// CHECK:      [[MAC_FN:%.*]] = function_ref @_TF6errors10make_a_catFzT_CS_3Cat : $@convention(thin) () -> (@owned Cat, @error _ErrorType)
// CHECK-NEXT: try_apply [[MAC_FN]]() : $@convention(thin) () -> (@owned Cat, @error _ErrorType), normal [[MAC_NORMAL:bb[0-9]+]], error [[MAC_ERROR:bb[0-9]+]]
// CHECK:    [[MAC_NORMAL]]([[T0:%.*]] : $Cat):
// CHECK-NEXT: br [[TERNARY_CONT:bb[0-9]+]]([[T0]] : $Cat)

//   In the false case, call dont_make_a_cat().
// CHECK:    [[FLAG_FALSE]]:
// CHECK:      [[DMAC_FN:%.*]] = function_ref @_TF6errors15dont_make_a_catFzT_CS_3Cat : $@convention(thin) () -> (@owned Cat, @error _ErrorType)
// CHECK-NEXT: try_apply [[DMAC_FN]]() : $@convention(thin) () -> (@owned Cat, @error _ErrorType), normal [[DMAC_NORMAL:bb[0-9]+]], error [[DMAC_ERROR:bb[0-9]+]]
// CHECK:    [[DMAC_NORMAL]]([[T0:%.*]] : $Cat):
// CHECK-NEXT: br [[TERNARY_CONT]]([[T0]] : $Cat)

//   Merge point for the ternary operator.  Call dont_return with the result.
// CHECK:    [[TERNARY_CONT]]([[T0:%.*]] : $Cat):
// CHECK-NEXT: [[ARG_TEMP:%.*]] = alloc_stack $Cat
// CHECK-NEXT: store [[T0]] to [[ARG_TEMP]]#1
// CHECK-NEXT: [[RET_TEMP:%.*]] = alloc_stack $Cat
// CHECK-NEXT: try_apply [[DR_FN]]<Cat>([[RET_TEMP]]#1, [[ARG_TEMP]]#1) : $@convention(thin) <τ_0_0> (@out τ_0_0, @in τ_0_0) -> @error _ErrorType, normal [[DR_NORMAL:bb[0-9]+]], error [[DR_ERROR:bb[0-9]+]]
// CHECK:    [[DR_NORMAL]]({{%.*}} : $()):
// CHECK-NEXT: [[T0:%.*]] = load [[RET_TEMP]]#1 : $*Cat
// CHECK-NEXT: dealloc_stack [[RET_TEMP]]#0
// CHECK-NEXT: dealloc_stack [[ARG_TEMP]]#0
// CHECK-NEXT: br [[RETURN:bb[0-9]+]]([[T0]] : $Cat)

//   Return block.
// CHECK:    [[RETURN]]([[T0:%.*]] : $Cat):
// CHECK-NEXT: return [[T0]] : $Cat

//   Catch dispatch block.
// CHECK:    [[CATCH:bb[0-9]+]]([[ERROR:%.*]] : $_ErrorType):
// CHECK-NEXT: [[SRC_TEMP:%.*]] = alloc_stack $_ErrorType
// CHECK-NEXT: store [[ERROR]] to [[SRC_TEMP]]#1
// CHECK-NEXT: [[DEST_TEMP:%.*]] = alloc_stack $HomeworkError
// CHECK-NEXT: checked_cast_addr_br copy_on_success _ErrorType in [[SRC_TEMP]]#1 : $*_ErrorType to HomeworkError in [[DEST_TEMP]]#1 : $*HomeworkError, [[IS_HWE:bb[0-9]+]], [[NOT_HWE:bb[0-9]+]]

//   Catch HomeworkError.
// CHECK:    [[IS_HWE]]:
// CHECK-NEXT: [[T0:%.*]] = load [[DEST_TEMP]]#1 : $*HomeworkError
// CHECK-NEXT: switch_enum [[T0]] : $HomeworkError, case #HomeworkError.CatAteIt!enumelt.1: [[MATCH:bb[0-9]+]], default [[NO_MATCH:bb[0-9]+]]

//   Catch HomeworkError.CatAteIt.
// CHECK:    [[MATCH]]([[T0:%.*]] : $Cat):
// CHECK-NEXT: debug_value
// CHECK-NEXT: dealloc_stack [[DEST_TEMP]]#0
// CHECK-NEXT: destroy_addr [[SRC_TEMP]]#1
// CHECK-NEXT: dealloc_stack [[SRC_TEMP]]#0
// CHECK-NEXT: br [[RETURN]]([[T0]] : $Cat)

//   Catch other HomeworkErrors.
// CHECK:    [[NO_MATCH]]:
// CHECK-NEXT: dealloc_stack [[DEST_TEMP]]#0
// CHECK-NEXT: br [[CATCHALL:bb[0-9]+]]

//   Catch other types.
// CHECK:    [[NOT_HWE]]:
// CHECK-NEXT: dealloc_stack [[DEST_TEMP]]#0
// CHECK-NEXT: br [[CATCHALL:bb[0-9]+]]

//   Catch all.
// CHECK:    [[CATCHALL]]:
// CHECK-NEXT: dealloc_stack [[SRC_TEMP]]#0
// CHECK-NEXT: br [[CATCHALL:bb[0-9]+]]
// CHECK:    [[CATCHALL]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFC6errors3CatCfMS0_FT_S0_ : $@convention(thin) (@thick Cat.Type) -> @owned Cat
// CHECK-NEXT: [[T1:%.*]] = metatype $@thick Cat.Type
// CHECK-NEXT: [[T2:%.*]] = apply [[T0]]([[T1]])
// CHECK-NEXT: strong_release [[ERROR]] : $_ErrorType
// CHECK-NEXT: br [[RETURN]]([[T2]] : $Cat)

//   Landing pad.
// CHECK:    [[MAC_ERROR]]([[T0:%.*]] : $_ErrorType):
// CHECK-NEXT: br [[CATCH]]([[T0]] : $_ErrorType)
// CHECK:    [[DMAC_ERROR]]([[T0:%.*]] : $_ErrorType):
// CHECK-NEXT: br [[CATCH]]([[T0]] : $_ErrorType)
// CHECK:    [[DR_ERROR]]([[T0:%.*]] : $_ErrorType):
// CHECK-NEXT: dealloc_stack
// CHECK-NEXT: dealloc_stack
// CHECK-NEXT: br [[CATCH]]([[T0]] : $_ErrorType)
func all_together_now(flag: Bool) -> Cat {
  do {
    return try dont_return(flag ? make_a_cat() : dont_make_a_cat())
  } catch HomeworkError.CatAteIt(let cat) {
    return cat
  } catch _ {
    return Cat()
  }
}

// Initializers.
class HasThrowingInit {
  var field: Int
  init(value: Int) throws {
    field = value
  }
}
// CHECK-LABEL: sil hidden @_TFC6errors15HasThrowingInitcfMS0_FzT5valueSi_S0_ : $@convention(method) (Int, @owned HasThrowingInit) -> (@owned HasThrowingInit, @error _ErrorType) {
// CHECK:      [[T0:%.*]] = mark_uninitialized [rootself] %1 : $HasThrowingInit
// CHECK-NEXT: [[T1:%.*]] = ref_element_addr [[T0]] : $HasThrowingInit
// CHECK-NEXT: assign %0 to [[T1]] : $*Int
// CHECK-NEXT: return [[T0]] : $HasThrowingInit

// CHECK-LABEL: sil hidden @_TFC6errors15HasThrowingInitCfMS0_FzT5valueSi_S0_ : $@convention(thin) (Int, @thick HasThrowingInit.Type) -> (@owned HasThrowingInit, @error _ErrorType)
// CHECK:      [[SELF:%.*]] = alloc_ref $HasThrowingInit
// CHECK:      [[T0:%.*]] = function_ref @_TFC6errors15HasThrowingInitcfMS0_FzT5valueSi_S0_ : $@convention(method) (Int, @owned HasThrowingInit) -> (@owned HasThrowingInit, @error _ErrorType)
// CHECK-NEXT: try_apply [[T0]](%0, [[SELF]]) : $@convention(method) (Int, @owned HasThrowingInit) -> (@owned HasThrowingInit, @error _ErrorType), normal bb1, error bb2
// CHECK:    bb1([[SELF:%.*]] : $HasThrowingInit):
// CHECK-NEXT: return [[SELF]]
// CHECK:    bb2([[ERROR:%.*]] : $_ErrorType):
// CHECK-NEXT: builtin "willThrow"
// CHECK-NEXT: throw [[ERROR]]


enum ColorError : _ErrorType {
  case Red, Green, Blue
}

//CHECK-LABEL: sil hidden @_TF6errors6IThrowFzT_VSs5Int32
//CHECK: builtin "willThrow"
//CHECK-NEXT: throw
//CHECK: return
func IThrow() throws -> Int32 {
  throw ColorError.Red
  return 0
}

// Make sure that we are not emitting calls to 'willThrow' on rethrow sites.
//CHECK-LABEL: sil hidden @_TF6errors12DoesNotThrowFzT_VSs5Int32
//CHECK-NOT: builtin "willThrow"
//CHECK: return
func DoesNotThrow() throws -> Int32 {
  try IThrow()
  return 2
}

// rdar://20782111
protocol Doomed {
  func check() throws
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV6errors12DoomedStructS_6DoomedS_FS1_5checkUS1___fQPS1_FzT_T_ : $@convention(witness_method) (@in_guaranteed DoomedStruct) -> @error _ErrorType
// CHECK:      [[TEMP:%.*]] = alloc_stack $DoomedStruct
// CHECK:      copy_addr %0 to [initialization] [[TEMP]]#1
// CHECK:      [[SELF:%.*]] = load [[TEMP]]#1 : $*DoomedStruct
// CHECK:      [[T0:%.*]] = function_ref @_TFV6errors12DoomedStruct5checkfS0_FzT_T_ : $@convention(method) (DoomedStruct) -> @error _ErrorType
// CHECK-NEXT: try_apply [[T0]]([[SELF]])
// CHECK:    bb1([[T0:%.*]] : $()):
// CHECK:      dealloc_stack [[TEMP]]#0
// CHECK:      return [[T0]] : $()
// CHECK:    bb2([[T0:%.*]] : $_ErrorType):
// CHECK:      builtin "willThrow"([[T0]] : $_ErrorType)
// CHECK:      dealloc_stack [[TEMP]]#0
// CHECK:      throw [[T0]] : $_ErrorType
struct DoomedStruct : Doomed {
  func check() throws {}
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC6errors11DoomedClassS_6DoomedS_FS1_5checkUS1___fQPS1_FzT_T_ : $@convention(witness_method) (@in_guaranteed DoomedClass) -> @error _ErrorType {
// CHECK:      [[TEMP:%.*]] = alloc_stack $DoomedClass
// CHECK:      copy_addr %0 to [initialization] [[TEMP]]#1
// CHECK:      [[SELF:%.*]] = load [[TEMP]]#1 : $*DoomedClass
// CHECK:      [[T0:%.*]] = class_method [[SELF]] : $DoomedClass, #DoomedClass.check!1 : DoomedClass -> () throws -> () , $@convention(method) (@guaranteed DoomedClass) -> @error _ErrorType
// CHECK-NEXT: try_apply [[T0]]([[SELF]])
// CHECK:    bb1([[T0:%.*]] : $()):
// CHECK:      strong_release [[SELF]] : $DoomedClass
// CHECK:      dealloc_stack [[TEMP]]#0
// CHECK:      return [[T0]] : $()
// CHECK:    bb2([[T0:%.*]] : $_ErrorType):
// CHECK:      builtin "willThrow"([[T0]] : $_ErrorType)
// CHECK:      strong_release [[SELF]] : $DoomedClass
// CHECK:      dealloc_stack [[TEMP]]#0
// CHECK:      throw [[T0]] : $_ErrorType
class DoomedClass : Doomed {
  func check() throws {}
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV6errors11HappyStructS_6DoomedS_FS1_5checkUS1___fQPS1_FzT_T_ : $@convention(witness_method) (@in_guaranteed HappyStruct) -> @error _ErrorType
// CHECK:      [[TEMP:%.*]] = alloc_stack $HappyStruct
// CHECK:      copy_addr %0 to [initialization] [[TEMP]]#1
// CHECK:      [[SELF:%.*]] = load [[TEMP]]#1 : $*HappyStruct
// CHECK:      [[T0:%.*]] = function_ref @_TFV6errors11HappyStruct5checkfS0_FT_T_ : $@convention(method) (HappyStruct) -> ()
// CHECK:      [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:      dealloc_stack [[TEMP]]#0
// CHECK:      return [[T1]] : $()
struct HappyStruct : Doomed {
  func check() {}
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC6errors10HappyClassS_6DoomedS_FS1_5checkUS1___fQPS1_FzT_T_ : $@convention(witness_method) (@in_guaranteed HappyClass) -> @error _ErrorType
// CHECK:      [[TEMP:%.*]] = alloc_stack $HappyClass
// CHECK:      copy_addr %0 to [initialization] [[TEMP]]#1
// CHECK:      [[SELF:%.*]] = load [[TEMP]]#1 : $*HappyClass
// CHECK:      [[T0:%.*]] = class_method [[SELF]] : $HappyClass, #HappyClass.check!1 : HappyClass -> () -> () , $@convention(method) (@guaranteed HappyClass) -> ()
// CHECK:      [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:      strong_release [[SELF]] : $HappyClass
// CHECK:      dealloc_stack [[TEMP]]#0
// CHECK:      return [[T1]] : $()
class HappyClass : Doomed {
  func check() {}
}

func create<T>(fn: () throws -> T) throws -> T {
  return try fn()
}
func testThunk(fn: () throws -> Int) throws -> Int {
  return try create(fn)
}
// CHECK-LABEL: sil shared [transparent] [thunk] @_TTRXFo__dSi_XFo__iSi_ : $@convention(thin) (@out Int, @owned @callee_owned () -> (Int, @error _ErrorType)) -> @error _ErrorType
// CHECK: bb0(%0 : $*Int, %1 : $@callee_owned () -> (Int, @error _ErrorType)):
// CHECK:   try_apply %1()
// CHECK: bb1([[T0:%.*]] : $Int):
// CHECK:   store [[T0]] to %0 : $*Int
// CHECK:   [[T0:%.*]] = tuple ()
// CHECK:   return [[T0]]
// CHECK: bb2([[T0:%.*]] : $_ErrorType):
// CHECK:   builtin "willThrow"([[T0]] : $_ErrorType)
// CHECK:   throw [[T0]] : $_ErrorType
