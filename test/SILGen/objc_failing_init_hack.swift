// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -emit-silgen -module-cache-path %t/clang-module-cache -target x86_64-apple-macosx10.9 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck -check-prefix=SILGEN %s
// RUN: %swift -emit-sil -O -module-cache-path %t/clang-module-cache -target x86_64-apple-macosx10.9 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck -check-prefix=OPT %s

import Foundation

// SILGEN-LABEL: sil @_TF22objc_failing_init_hack10makeObjectFT_GSqCSo8NSObject_
// SILGEN:         [[INIT:%.*]] = function_ref @_TFCSo8NSObjectCfMS_FT_S_
// SILGEN:         [[NONOPTIONAL:%.*]] = apply [[INIT]]
// SILGEN:         [[OPTIONAL:%.*]] = unchecked_ref_bit_cast [[NONOPTIONAL]]

// OPT-LABEL: sil @_TF22objc_failing_init_hack10makeObjectFT_GSqCSo8NSObject_
// OPT:         [[OPT:%.*]] = unchecked_ref_bit_cast
// OPT:         switch_enum [[OPT]] : $Optional<NSObject>, case #Optional.None!enumelt: [[NIL:bb[0-9]+]]
// OPT:       [[NIL]]:
// OPT:         string_literal utf16 "nil"

func makeObject() -> NSObject? {
  let foo: NSObject? = NSObject()
  if foo == nil {
    println("nil")
  }
  return foo
}
