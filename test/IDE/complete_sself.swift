// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_BODY_EXPR | %FileCheck %s --check-prefix=NOSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_BODY_TYPE | %FileCheck %s --check-prefix=NOSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_VARBODY_EXPR | %FileCheck %s --check-prefix=NOSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_FUNC_PARAMTYPE | %FileCheck %s --check-prefix=NOSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_FUNC_DEFAULTEXPR | %FileCheck %s --check-prefix=NOSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_FUNC_RESULTTYPE | %FileCheck %s --check-prefix=NOSELF

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_FUNC_PARAMTYPE | %FileCheck %s --check-prefix=GENERICPARAM
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_FUNC_RESULTTYPE  | %FileCheck %s --check-prefix=GENERICPARAM
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_SUBSCRIPT_PARAMTYPE | %FileCheck %s --check-prefix=GENERICPARAM
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_SUBSCRIPT_RESULTTYPE | %FileCheck %s --check-prefix=GENERICPARAM
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_VAR_TYPE | %FileCheck %s --check-prefix=GENERICPARAM

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOEXT_FUNC_PARAMTYPE | %FileCheck %s --check-prefix=GENERICPARAM
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOEXT_FUNC_DEFAULTEXPR | %FileCheck %s --check-prefix=GENERICPARAM
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOEXT_FUNC_RESULTTYPE  | %FileCheck %s --check-prefix=GENERICPARAM
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOEXT_SUBSCRIPT_PARAMTYPE | %FileCheck %s --check-prefix=GENERICPARAM
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOEXT_SUBSCRIPT_RESULTTYPE | %FileCheck %s --check-prefix=GENERICPARAM
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOEXT_VAR_TYPE | %FileCheck %s --check-prefix=GENERICPARAM
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOEXT_BODY_EXPR | %FileCheck %s --check-prefix=GENERICPARAM
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOEXT_BODY_TYPE | %FileCheck %s --check-prefix=GENERICPARAM
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOEXT_VARBODY_EXPR | %FileCheck %s --check-prefix=GENERICPARAM

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_FUNC_PARAMTYPE | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_FUNC_DEFAULTEXPR | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_FUNC_RESULTTYPE  | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_SUBSCRIPT_PARAMTYPE | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_SUBSCRIPT_RESULTTYPE | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_VAR_TYPE | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_BODY_EXPR | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_BODY_TYPE | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_VARBODY_EXPR | %FileCheck %s --check-prefix=STATICSELF

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCTEXT_FUNC_PARAMTYPE | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCTEXT_FUNC_DEFAULTEXPR | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCTEXT_FUNC_RESULTTYPE  | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCTEXT_SUBSCRIPT_PARAMTYPE | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCTEXT_SUBSCRIPT_RESULTTYPE | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCTEXT_VAR_TYPE | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCTEXT_BODY_EXPR | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCTEXT_BODY_TYPE | %FileCheck %s --check-prefix=STATICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCTEXT_VARBODY_EXPR | %FileCheck %s --check-prefix=STATICSELF

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_FUNC_PARAMTYPE | %FileCheck %s --check-prefix=NOSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_FUNC_DEFAULTEXPR | %FileCheck %s --check-prefix=NOSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_FUNC_RESULTTYPE  | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_SUBSCRIPT_PARAMTYPE | %FileCheck %s --check-prefix=NOSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_SUBSCRIPT_RESULTTYPE | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_VAR_TYPE | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BODY_EXPR | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BODY_TYPE | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_VARBODY_EXPR | %FileCheck %s --check-prefix=DYNAMICSELF

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASSEXT_FUNC_PARAMTYPE | %FileCheck %s --check-prefix=NOSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASSEXT_FUNC_DEFAULTEXPR | %FileCheck %s --check-prefix=NOSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASSEXT_FUNC_RESULTTYPE  | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASSEXT_SUBSCRIPT_PARAMTYPE | %FileCheck %s --check-prefix=NOSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASSEXT_SUBSCRIPT_RESULTTYPE | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASSEXT_VAR_TYPE | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASSEXT_BODY_EXPR | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASSEXT_BODY_TYPE | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASSEXT_VARBODY_EXPR | %FileCheck %s --check-prefix=DYNAMICSELF

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_NESTEDBODY_TYPE | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_NESTEDBODY_EXPR | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_NESTEDFUNC_PARAMTYPE | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_NESTEDFUNC_DEFAULTEXPR | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_NESTEDFUNC_RESULTTYPE | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_TYPEALIAS_TYPE | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_NESTEDTYPE_EXPR | %FileCheck %s --check-prefix=DYNAMICSELF
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_NESTEDTYPE_TYPE | %FileCheck %s --check-prefix=DYNAMICSELF

// NOSELF: Begin completions
// NOSELF-NOT: name=Self
// NOSELF: End completions

// GENERICPARAM: Begin completions
// GENERICPARAM: Decl[GenericTypeParam]/Local: Self[#Self#];

// STATICSELF: Begin completions
// STATICSELF: Keyword[Self]/CurrNominal: Self[#S#];

// DYNAMICSELF: Begin completions
// DYNAMICSELF: Keyword[Self]/CurrNominal: Self[#Self#];

func freeFunc() {
  #^GLOBAL_BODY_EXPR^#
  let _: #^GLOBAL_BODY_TYPE^#
}
var freeVar: String {
  "\(#^GLOBAL_VARBODY_EXPR^#)"
}
func freeFunc(x: #^GLOBAL_FUNC_PARAMTYPE^#) {}
func freeFunc(x: Int = #^GLOBAL_FUNC_DEFAULTEXPR^#) {}
func freeFunc(x: Int) -> #^GLOBAL_FUNC_RESULTTYPE^# {}

var x: ^#GLOBAL_VAR_TYPE^#

func sync() {}

protocol P {
  func protoMeth(x: #^PROTOCOL_FUNC_PARAMTYPE^#)
  func protoMeth(x: Int) -> #^PROTOCOL_FUNC_RESULTTYPE^#

  subscript(x: #^PROTOCOL_SUBSCRIPT_PARAMTYPE^#) -> Int { get }
  subscript(y: Int) -> #^PROTOCOL_SUBSCRIPT_RESULTTYPE^# { get }
  
  var x: #^PROTOCOL_VAR_TYPE^#
}
extension P {
  func method(x: #^PROTOEXT_FUNC_PARAMTYPE^#) { }
  func method(x: Int = #^PROTOEXT_FUNC_DEFAULTEXPR^#) { }
  func method(x: Int) -> #^PROTOEXT_FUNC_RESULTTYPE^# { }

  subscript(x: #^PROTOEXT_SUBSCRIPT_PARAMTYPE^#) -> Int { }
  subscript(y: Int) -> #^PROTOEXT_SUBSCRIPT_RESULTTYPE^# { }
  
  var x: #^PROTOEXT_VAR_TYPE^# { }

  func bodyTest() {
    #^PROTOEXT_BODY_EXPR^#
    let _: #^PROTOEXT_BODY_TYPE^#
  }
  var varTest: String {
    "\(#^PROTOEXT_VARBODY_EXPR^#)"
  }
}

struct S {
  func method(x: #^STRUCT_FUNC_PARAMTYPE^#)
  func method(x: Int = #^STRUCT_FUNC_DEFAULTEXPR^#) { }
  func method(x: Int) -> #^STRUCT_FUNC_RESULTTYPE^#

  subscript(x: #^STRUCT_SUBSCRIPT_PARAMTYPE^#) -> Int { get }
  subscript(y: Int) -> #^STRUCT_SUBSCRIPT_RESULTTYPE^# { get }
  
  var x: #^STRUCT_VAR_TYPE^#

  func bodyTest() {
    #^STRUCT_BODY_EXPR^#
    let _: #^STRUCT_BODY_TYPE^#
  }
  var varTest: String {
    "\(#^STRUCT_VARBODY_EXPR^#)"
  }
}
extension S {
  func method(x: #^STRUCTEXT_FUNC_PARAMTYPE^#)
  func method(x: Int = #^STRUCTEXT_FUNC_DEFAULTEXPR^#) { }
  func method(x: Int) -> #^STRUCTEXT_FUNC_RESULTTYPE^#

  subscript(x: #^STRUCTEXT_SUBSCRIPT_PARAMTYPE^#) -> Int { get }
  subscript(y: Int) -> #^STRUCTEXT_SUBSCRIPT_RESULTTYPE^# { get }
  
  var x: #^STRUCTEXT_VAR_TYPE^#

  func bodyTest() {
    #^STRUCTEXT_BODY_EXPR^#
    let _: #^STRUCTEXT_BODY_TYPE^#
  }
  var varTest: String {
    "\(#^STRUCTEXT_VARBODY_EXPR^#)"
  }
}

class C {
  func method(x: #^CLASS_FUNC_PARAMTYPE^#)
  func method(x: Int = #^CLASS_FUNC_DEFAULTEXPR^#) { }
  func method(x: Int) -> #^CLASS_FUNC_RESULTTYPE^#

  subscript(x: #^CLASS_SUBSCRIPT_PARAMTYPE^#) -> Int { get }
  subscript(y: Int) -> #^CLASS_SUBSCRIPT_RESULTTYPE^# { get }
  
  var x: #^CLASS_VAR_TYPE^#

  func bodyTest() {
    #^CLASS_BODY_EXPR^#
    let _: #^CLASS_BODY_TYPE^#
  }
  var varTest: String {
    "\(#^CLASS_VARBODY_EXPR^#)"
  }
}
class CC {}
extension CC {
  func method(x: #^CLASSEXT_FUNC_PARAMTYPE^#)
  func method(x: Int = #^CLASSEXT_FUNC_DEFAULTEXPR^#) { }
  func method(x: Int) -> #^CLASSEXT_FUNC_RESULTTYPE^#

  subscript(x: #^CLASSEXT_SUBSCRIPT_PARAMTYPE^#) -> Int { get }
  subscript(y: Int) -> #^CLASSEXT_SUBSCRIPT_RESULTTYPE^# { get }
  
  var x: #^CLASSEXT_VAR_TYPE^#

  func bodyTest() {
    #^CLASSEXT_BODY_EXPR^#
    let _: #^CLASSEXT_BODY_TYPE^#
  }
  var varTest: String {
    "\(#^CLASSEXT_VARBODY_EXPR^#)"
  }
}

class CCC {
    func bodyTest() {
      func inner() {
        #^CLASS_NESTEDBODY_EXPR^#
        let _: #^CLASS_NESTEDBODY_TYPE^#
      }

      func inner(x: #^CLASS_NESTEDFUNC_PARAMTYPE^#) {}
      func inner(y: Int = #^CLASS_NESTEDFUNC_DEFAULTEXPR^#) {}
      func inner() -> #^CLASS_NESTEDFUNC_RESULTTYPE^# {}

      typealias A<T> = #^CLASS_TYPEALIAS_TYPE^#
    }
    class Inner {
        func method() {
            #^CLASS_NESTEDTYPE_EXPR^#
            let _: #^CLASS_NESTEDTYPE_TYPE^#
        }
    }
}
