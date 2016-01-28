// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AFTER_POUND | FileCheck %s

// REQUIRES: objc_interop

{
  if ##^AFTER_POUND^#
}

// CHECK: Keyword/ExprSpecific:               available({#Platform...#}, *); name=available(Platform..., *)
// CHECK: Keyword/ExprSpecific:               selector({#@objc method#}); name=selector(@objc method)
