// RUN: %target-typecheck-verify-swift

@_semantics("foo")
@_semantics("bar")
func duplicatesemantics() {}

func func_with_nested_semantics_1() {
   @_semantics("exit") 
   func exit(_ code : UInt32) -> Void
   exit(0)
}

// Test parser recovery by having something that
// should parse fine.
func somethingThatShouldParseFine() {}

func func_with_nested_semantics_2() {
   @_semantics("exit") 
   func exit(_ code : UInt32) -> Void
   exit(0)
}

@_semantics("struct")
struct StructWithSemantics {}

@_semantics("class")
class ClassWithSemantics {}

@_semantics("enum")
enum EnumWithSemantics {}

@_semantics("struct1")
@_semantics("struct2")
struct StructWithDuplicateSemantics {}

@_semantics("globalVar1")
@_semantics("globalVar2")
var globalVarWithSemantics : Int = 5

@_semantics("globalLet1")
@_semantics("globalLet2")
let globalLetWithSemantics : Int = 5

func varDeclLocalVars() {
  @_semantics("localVar1")
  @_semantics("localVar2")
  var localVarWithSemantics : Int = 5
  localVarWithSemantics = 6
  let _ = localVarWithSemantics
  
  @_semantics("localLet1")
  @_semantics("localLet2")
  let localLetWithSemantics : Int = 5
  let _ = localLetWithSemantics
}

struct IVarTest {
  @_semantics("localVar1")
  @_semantics("localVar2")
  var localVarWithSemantics : Int = 5
  
  @_semantics("localLet1")
  @_semantics("localLet2")
  let localLetWithSemantics : Int = 5
}
