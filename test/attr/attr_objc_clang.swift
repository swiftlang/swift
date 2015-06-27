// RUN: %target-parse-verify-swift -sdk %S/Inputs -I %S/Inputs/custom-modules
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s -sdk %S/Inputs -I %S/Inputs/custom-modules -function-definitions=true -prefer-type-repr=false -print-implicit-attrs=true -explode-pattern-binding-decls=true -disable-objc-attr-requires-foundation-module  | FileCheck %s

// REQUIRES: objc_interop

import AttrObjc_FooClangModule

@objc
class infer_instanceVar1 {
// CHECK-LABEL: @objc class infer_instanceVar1 {

  var var_ClangEnum: FooEnum1
  var var_ClangStruct: FooStruct1
// CHECK-LABEL: @objc var var_ClangEnum: FooEnum1
// CHECK-LABEL: @objc var var_ClangStruct: FooStruct1

  init(fe: FooEnum1, fs: FooStruct1) {
    var_ClangEnum = fe
    var_ClangStruct = fs
  }
}

class ObjC_Class1 : NSObject, Hashable { 
  var hashValue: Int { return 0 }
}

func ==(lhs: ObjC_Class1, rhs: ObjC_Class1) -> Bool {
  return true
}

@objc class ObjC_Class2 : Hashable { 
  var hashValue: Int { return 0 }
}

func ==(lhs: ObjC_Class2, rhs: ObjC_Class2) -> Bool {
  return true
}

@objc class DictionaryTest {
  // CHECK-LABEL: @objc func func_dictionary1a(x: Dictionary<ObjC_Class1, ObjC_Class1>)
  func func_dictionary1a(x: Dictionary<ObjC_Class1, ObjC_Class1>) { }

  // CHECK-LABEL: @objc func func_dictionary1b(x: Dictionary<ObjC_Class1, ObjC_Class1>)
  @objc func func_dictionary1b(x: Dictionary<ObjC_Class1, ObjC_Class1>) { }

  // CHECK-LABEL: @objc func func_dictionary2a(x: Dictionary<ObjC_Class1, ObjC_Class2>)
  func func_dictionary2a(x: Dictionary<ObjC_Class1, ObjC_Class2>) { }

  // CHECK-LABEL: @objc func func_dictionary2b(x: Dictionary<ObjC_Class1, ObjC_Class2>)
  @objc func func_dictionary2b(x: Dictionary<ObjC_Class1, ObjC_Class2>) { }

  // FIXME: Should be okay.
  // FIXME: func func_dictionary3a(x: Dictionary<String, Int>) { }
  // FIXME: @objc func func_dictionary3b(x: Dictionary<String, Int>) { }
}
