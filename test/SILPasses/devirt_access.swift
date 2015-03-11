// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module -Onone -o %t %S/Inputs/devirt_access_other_module.swift

// RUN: %target-swift-frontend -O -primary-file %s %S/Inputs/devirt_access_helper.swift -I %t -emit-sil -sil-inline-threshold 1000 -sil-verify-all | FileCheck -check-prefix=WHOLE-MODULE %s
// RUN: %target-swift-frontend -O -primary-file %s %S/Inputs/devirt_access_helper.swift -I %t -emit-sil -sil-inline-threshold 1000 -sil-verify-all | FileCheck -check-prefix=PRIMARY-FILE %s

import devirt_access_other_module


//PRIMARY-FILE-LABEL: sil hidden @_TF13devirt_access19testExternalPrivateFT_T_
//PRIMARY-FILE: class_method
//WHOLE-MODULE-LABEL: sil hidden @_TF13devirt_access19testExternalPrivateFT_T_
//WHOLE-MODULE: class_method
func testExternalPrivate() {
  let obj = getExternalClass()
  invokeFoo(obj)
}

//PRIMARY-FILE-LABEL: sil hidden @_TF13devirt_access20testInternalInternalFT_T_
//PRIMARY-FILE: class_method 
//WHOLE-MODULE-LABEL: sil hidden @_TF13devirt_access20testInternalInternalFT_T_
//WHOLE-MODULE: function_ref @_TF13devirt_access16getInternalClassFT_CS_13InternalClass
//WHOLE-MODULE: return
func testInternalInternal() {
  let obj = getInternalClass()
  obj.bar()
}

//PRIMARY-FILE-LABEL: sil hidden @_TF13devirt_access19testInternalPrivateFT_T_
//PRIMARY-FILE: function_ref @_TF13devirt_access16getInternalClassFT_CS_13InternalClass 
//PRIMARY-FILE: function_ref @_TF13devirt_access9invokeFooFCS_13InternalClassT_ 
//PRIMARY-FILE: return
//WHOLE-MODULE-LABEL: sil hidden @_TF13devirt_access19testInternalPrivateFT_T_
//WHOLE-MODULE: function_ref @_TF13devirt_access16getInternalClassFT_CS_13InternalClass
//WHOLE-MODULE: return
func testInternalPrivate() {
  let obj = getInternalClass()
  invokeFoo(obj)
}


class LocalInternalClass {
  private func foo() {}
  func bar() {}
}

// Marked @inline(never) to keep from devirtualizing based on this.
@inline(never) func getLocalInternalClass() -> LocalInternalClass {
  return LocalInternalClass()
}

//PRIMARY-FILE-LABEL: sil hidden @_TF13devirt_access17testLocalInternalFT_T_
//PRIMARY-FILE: class_method
//WHOLE-MODULE-LABEL: sil hidden @_TF13devirt_access17testLocalInternalFT_T_
//WHOLE-MODULE: function_ref @_TF13devirt_access21getLocalInternalClassFT_CS_18LocalInternalClass
//WHOLE-MODULE: return
func testLocalInternal() {
  let obj = getLocalInternalClass()
  obj.bar()
}

//PRIMARY-FILE-LABEL: sil hidden @_TF13devirt_access16testLocalPrivateFT_T_
//PRIMARY-FILE: function_ref @_TF13devirt_access21getLocalInternalClassFT_CS_18LocalInternalClass
//PRIMARY-FILE: return
//WHOLE-MODULE-LABEL: sil hidden @_TF13devirt_access16testLocalPrivateFT_T_
//WHOLE-MODULE: function_ref @_TF13devirt_access21getLocalInternalClassFT_CS_18LocalInternalClass
//WHOLE-MODULE: return
func testLocalPrivate() {
  let obj = getLocalInternalClass()
  obj.foo()
}


private class PrivateClass {
  private func foo() {}
  private func bar() {}
}

private class LocalPrivateSubclass : PrivateClass {
  override func foo() {}
}

// Marked @inline(never) to keep from devirtualizing based on this.
@inline(never) private func getPrivateClass() -> PrivateClass {
  return LocalPrivateSubclass()
}

//PRIMARY-FILE-LABEL: sil hidden @_TF13devirt_access11testPrivateFT_T_
//PRIMARY-FILE: function_ref @_TF13devirt_accessP[[DISCRIMINATOR:[0-9]+_.+]]15getPrivateClassFT_CS_P[[DISCRIMINATOR]]12PrivateClass
//WHOLE-MODULE-LABEL: sil hidden @_TF13devirt_access11testPrivateFT_T_
//WHOLE-MODULE: function_ref @_TF13devirt_accessP[[DISCRIMINATOR:[0-9]+_.+]]15getPrivateClassFT_CS_P[[DISCRIMINATOR]]12PrivateClass
//WHOLE-MODULE: return
func testPrivate() {
  let obj = getPrivateClass()
  obj.foo()
}

//PRIMARY-FILE-LABEL: sil hidden @_TF13devirt_access21testPrivateOverriddenFT_T_
//PRIMARY-FILE: function_ref @_TF13devirt_accessP[[DISCRIMINATOR:[0-9]+_.+]]15getPrivateClassFT_CS_P[[DISCRIMINATOR]]12PrivateClass
//WHOLE-MODULE-LABEL: sil hidden @_TF13devirt_access21testPrivateOverriddenFT_T_
//WHOLE-MODULE: function_ref @_TF13devirt_accessP[[DISCRIMINATOR]]15getPrivateClassFT_CS_P[[DISCRIMINATOR]]12PrivateClass
//WHOLE-MODULE: return
func testPrivateOverridden() {
  let obj = getPrivateClass()
  obj.bar()
}
