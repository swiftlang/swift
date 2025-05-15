// RUN: %batch-code-completion

// NOSELF-NOT: name=Self

// GENERICPARAM: Decl[GenericTypeParam]/Local: Self[#Self#];

// STATICSELF: Keyword[Self]/CurrNominal{{(/TypeRelation\[Convertible\])?}}: Self[#S#];

// DYNAMICSELF: Keyword[Self]/CurrNominal{{(/TypeRelation\[Convertible\])?}}: Self[#Self#];

func freeFunc() {
  #^GLOBAL_BODY_EXPR?check=NOSELF^#
  let _: #^GLOBAL_BODY_TYPE?check=NOSELF^#
}
var freeVar: String {
  "\(#^GLOBAL_VARBODY_EXPR?check=NOSELF^#)"
}
func freeFunc(x: #^GLOBAL_FUNC_PARAMTYPE?check=NOSELF^#) {}
func freeFunc(x: Int = #^GLOBAL_FUNC_DEFAULTEXPR?check=NOSELF^#) {}
func freeFunc(x: Int) -> #^GLOBAL_FUNC_RESULTTYPE?check=NOSELF^# {}

var x: ^#GLOBAL_VAR_TYPE^#

func sync() {}

protocol P {
  func protoMeth(x: #^PROTOCOL_FUNC_PARAMTYPE?check=GENERICPARAM^#)
  func protoMeth(x: Int) -> #^PROTOCOL_FUNC_RESULTTYPE?check=GENERICPARAM^#

  subscript(x: #^PROTOCOL_SUBSCRIPT_PARAMTYPE?check=GENERICPARAM^#) -> Int { get }
  subscript(y: Int) -> #^PROTOCOL_SUBSCRIPT_RESULTTYPE?check=GENERICPARAM^# { get }
  
  var x: #^PROTOCOL_VAR_TYPE?check=GENERICPARAM^#
}
extension P {
  func method(x: #^PROTOEXT_FUNC_PARAMTYPE?check=GENERICPARAM^#) { }
  func method(x: Int = #^PROTOEXT_FUNC_DEFAULTEXPR?check=GENERICPARAM^#) { }
  func method(x: Int) -> #^PROTOEXT_FUNC_RESULTTYPE?check=GENERICPARAM^# { }

  subscript(x: #^PROTOEXT_SUBSCRIPT_PARAMTYPE?check=GENERICPARAM^#) -> Int { }
  subscript(y: Int) -> #^PROTOEXT_SUBSCRIPT_RESULTTYPE?check=GENERICPARAM^# { }
  
  var x: #^PROTOEXT_VAR_TYPE?check=GENERICPARAM^# { }

  func bodyTest() {
    #^PROTOEXT_BODY_EXPR?check=GENERICPARAM^#
    let _: #^PROTOEXT_BODY_TYPE?check=GENERICPARAM^#
  }
  var varTest: String {
    "\(#^PROTOEXT_VARBODY_EXPR?check=GENERICPARAM^#)"
  }
}

struct S {
  func method(x: #^STRUCT_FUNC_PARAMTYPE?check=STATICSELF^#)
  func method(x: Int = #^STRUCT_FUNC_DEFAULTEXPR?check=STATICSELF^#) { }
  func method(x: Int) -> #^STRUCT_FUNC_RESULTTYPE?check=STATICSELF^#

  subscript(x: #^STRUCT_SUBSCRIPT_PARAMTYPE?check=STATICSELF^#) -> Int { get }
  subscript(y: Int) -> #^STRUCT_SUBSCRIPT_RESULTTYPE?check=STATICSELF^# { get }
  
  var x: #^STRUCT_VAR_TYPE?check=STATICSELF^#

  func bodyTest() {
    #^STRUCT_BODY_EXPR?check=STATICSELF^#
    let _: #^STRUCT_BODY_TYPE?check=STATICSELF^#
  }
  var varTest: String {
    "\(#^STRUCT_VARBODY_EXPR?check=STATICSELF^#)"
  }
}
extension S {
  func method(x: #^STRUCTEXT_FUNC_PARAMTYPE?check=STATICSELF^#)
  func method(x: Int = #^STRUCTEXT_FUNC_DEFAULTEXPR?check=STATICSELF^#) { }
  func method(x: Int) -> #^STRUCTEXT_FUNC_RESULTTYPE?check=STATICSELF^#

  subscript(x: #^STRUCTEXT_SUBSCRIPT_PARAMTYPE?check=STATICSELF^#) -> Int { get }
  subscript(y: Int) -> #^STRUCTEXT_SUBSCRIPT_RESULTTYPE?check=STATICSELF^# { get }
  
  var x: #^STRUCTEXT_VAR_TYPE?check=STATICSELF^#

  func bodyTest() {
    #^STRUCTEXT_BODY_EXPR?check=STATICSELF^#
    let _: #^STRUCTEXT_BODY_TYPE?check=STATICSELF^#
  }
  var varTest: String {
    "\(#^STRUCTEXT_VARBODY_EXPR?check=STATICSELF^#)"
  }
}

class C {
  func method(x: #^CLASS_FUNC_PARAMTYPE?check=NOSELF^#)
  func method(x: Int = #^CLASS_FUNC_DEFAULTEXPR?check=NOSELF^#) { }
  func method(x: Int) -> #^CLASS_FUNC_RESULTTYPE?check=DYNAMICSELF^#

  subscript(x: #^CLASS_SUBSCRIPT_PARAMTYPE?check=NOSELF^#) -> Int { get }
  subscript(y: Int) -> #^CLASS_SUBSCRIPT_RESULTTYPE?check=DYNAMICSELF^# { get }
  
  var x: #^CLASS_VAR_TYPE?check=DYNAMICSELF^#

  func bodyTest() {
    #^CLASS_BODY_EXPR?check=DYNAMICSELF^#
    let _: #^CLASS_BODY_TYPE?check=DYNAMICSELF^#
  }
  var varTest: String {
    "\(#^CLASS_VARBODY_EXPR?check=DYNAMICSELF^#)"
  }
}
class CC {}
extension CC {
  func method(x: #^CLASSEXT_FUNC_PARAMTYPE?check=NOSELF^#)
  func method(x: Int = #^CLASSEXT_FUNC_DEFAULTEXPR?check=NOSELF^#) { }
  func method(x: Int) -> #^CLASSEXT_FUNC_RESULTTYPE?check=DYNAMICSELF^#

  subscript(x: #^CLASSEXT_SUBSCRIPT_PARAMTYPE?check=NOSELF^#) -> Int { get }
  subscript(y: Int) -> #^CLASSEXT_SUBSCRIPT_RESULTTYPE?check=DYNAMICSELF^# { get }
  
  var x: #^CLASSEXT_VAR_TYPE?check=DYNAMICSELF^#

  func bodyTest() {
    #^CLASSEXT_BODY_EXPR?check=DYNAMICSELF^#
    let _: #^CLASSEXT_BODY_TYPE?check=DYNAMICSELF^#
  }
  var varTest: String {
    "\(#^CLASSEXT_VARBODY_EXPR?check=DYNAMICSELF^#)"
  }
}

class CCC {
    func bodyTest() {
      func inner() {
        #^CLASS_NESTEDBODY_EXPR?check=DYNAMICSELF^#
        let _: #^CLASS_NESTEDBODY_TYPE?check=DYNAMICSELF^#
      }

      func inner(x: #^CLASS_NESTEDFUNC_PARAMTYPE?check=DYNAMICSELF^#) {}
      func inner(y: Int = #^CLASS_NESTEDFUNC_DEFAULTEXPR?check=DYNAMICSELF^#) {}
      func inner() -> #^CLASS_NESTEDFUNC_RESULTTYPE?check=DYNAMICSELF^# {}

      typealias A<T> = #^CLASS_TYPEALIAS_TYPE?check=DYNAMICSELF^#
    }
    class Inner {
        func method() {
            #^CLASS_NESTEDTYPE_EXPR?check=DYNAMICSELF^#
            let _: #^CLASS_NESTEDTYPE_TYPE?check=DYNAMICSELF^#
        }
    }
}
