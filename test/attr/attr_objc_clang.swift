// RUN: %target-typecheck-verify-swift -sdk %S/Inputs -I %S/Inputs/custom-modules -I %S/../Inputs/custom-modules
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s -sdk %S/Inputs -I %S/Inputs/custom-modules -I %S/../Inputs/custom-modules -function-definitions=true -prefer-type-repr=false -print-implicit-attrs=true -explode-pattern-binding-decls=true -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: objc_interop

import AttrObjc_FooClangModule
import ObjCRuntimeVisible

@objc
class infer_instanceVar1 {
// CHECK-LABEL: @objc class infer_instanceVar1 {

  @objc var var_ClangEnum: FooEnum1
  @objc var var_ClangStruct: FooStruct1
// CHECK-LABEL: @objc var var_ClangEnum: FooEnum1
// CHECK-LABEL: @objc var var_ClangStruct: FooStruct1

  @objc init(fe: FooEnum1, fs: FooStruct1) {
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

extension A {
  // CHECK: {{^}} func foo()
  func foo() { }
}
