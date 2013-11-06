// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift %s -verify -I %S/Inputs/custom-modules -module-cache-path=%t/clang-module-cache
// RUN: %swift-ide-test -print-ast-typechecked -source-filename %s -I %S/Inputs/custom-modules -module-cache-path=%t/clang-module-cache | FileCheck %s

import AttrObjc_FooClangModule

@objc
class infer_instanceVar1 {
// CHECK-LABEL: @objc class infer_instanceVar1 {
  var var_ClangEnum: FooEnum1
  var var_ClangStruct: FooStruct1
// CHECK-LABEL: var /* @objc(inferred) */ var_ClangEnum: FooEnum1
// CHECK-LABEL: var /* @objc(inferred) */ var_ClangStruct: FooStruct1
}

