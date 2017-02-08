// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module -Onone -o %t %S/Inputs/devirt_access_other_module.swift

// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -O -primary-file %s %S/Inputs/devirt_access_helper.swift -I %t -emit-sil -sil-inline-threshold 1000 -sil-verify-all | %FileCheck -check-prefix=WHOLE-MODULE %s
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -O -primary-file %s %S/Inputs/devirt_access_helper.swift -I %t -emit-sil -sil-inline-threshold 1000 -sil-verify-all | %FileCheck -check-prefix=PRIMARY-FILE %s

//PRIMARY-FILE-LABEL: sil hidden @_T013devirt_access012testInternalD0yyF
//PRIMARY-FILE: class_method 
//WHOLE-MODULE-LABEL: sil hidden @_T013devirt_access012testInternalD0yyF
//WHOLE-MODULE: function_ref @_T013devirt_access16getInternalClassAA0dE0CyF
//WHOLE-MODULE: return
func testInternalInternal() {
  let obj = getInternalClass()
  obj.bar()
}


class LocalInternalClass {
  fileprivate func foo() {}
  func bar() {}
}

// Marked @inline(never) to keep from devirtualizing based on this.
@inline(never) func getLocalInternalClass() -> LocalInternalClass {
  return LocalInternalClass()
}

//PRIMARY-FILE-LABEL: sil hidden @_T013devirt_access17testLocalInternalyyF
//PRIMARY-FILE: class_method
//WHOLE-MODULE-LABEL: sil hidden @_T013devirt_access17testLocalInternalyyF
//WHOLE-MODULE: function_ref @_T013devirt_access21getLocalInternalClassAA0deF0CyF
//WHOLE-MODULE: return
func testLocalInternal() {
  let obj = getLocalInternalClass()
  obj.bar()
}

//PRIMARY-FILE-LABEL: sil hidden @_T013devirt_access16testLocalPrivateyyF
//PRIMARY-FILE: function_ref @_T013devirt_access21getLocalInternalClassAA0deF0CyF
//PRIMARY-FILE: return
//WHOLE-MODULE-LABEL: sil hidden @_T013devirt_access16testLocalPrivateyyF
//WHOLE-MODULE: function_ref @_T013devirt_access21getLocalInternalClassAA0deF0CyF
//WHOLE-MODULE: return
func testLocalPrivate() {
  let obj = getLocalInternalClass()
  obj.foo()
}


private class PrivateClass {
  fileprivate func foo() {}
  fileprivate func bar() {}
}

private class LocalPrivateSubclass : PrivateClass {
  override func foo() {}
}

// Marked @inline(never) to keep from devirtualizing based on this.
@inline(never) private func getPrivateClass() -> PrivateClass {
  return LocalPrivateSubclass()
}

//PRIMARY-FILE-LABEL: sil hidden @_T013devirt_access11testPrivateyyF
//PRIMARY-FILE: function_ref @_T013devirt_access15getPrivateClass33_{{.*}}
//WHOLE-MODULE-LABEL: sil hidden @_T013devirt_access11testPrivateyyF
//WHOLE-MODULE: function_ref @_T013devirt_access15getPrivateClass33_{{.*}}
//WHOLE-MODULE: return
func testPrivate() {
  let obj = getPrivateClass()
  obj.foo()
}

//PRIMARY-FILE-LABEL: sil hidden @_T013devirt_access21testPrivateOverriddenyyF
//PRIMARY-FILE: function_ref @_T013devirt_access15getPrivateClass33_{{.*}}
//WHOLE-MODULE-LABEL: sil hidden @_T013devirt_access21testPrivateOverriddenyyF
//WHOLE-MODULE: function_ref @_T013devirt_access15getPrivateClass33_{{.*}}
//WHOLE-MODULE: return
func testPrivateOverridden() {
  let obj = getPrivateClass()
  obj.bar()
}
