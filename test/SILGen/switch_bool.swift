// RUN: %target-swift-emit-silgen -module-name switch_bool -import-objc-header %S/Inputs/switch_bool.h %s

// REQUIRES: objc_interop

// For now this test just makes sure that we do not crash on this pattern by
// running into dominance problems due to not-scoping the subcases of the
// SwitchValue. So performing a FileCheck test is not really needed. Feel free
// to add the FileCheck invocation if additional tests require FileCheck though.

func properlyScopeSubcasesOfEmitBoolDispatch() {
  let e = Optional<MyStringEnum>.none
  switch (e, false) {
  case (MyStringEnum.case1?, false):
    break
  case (let e?, _):
    let y = {
      return e
    }
  default:
    break
  }
}
