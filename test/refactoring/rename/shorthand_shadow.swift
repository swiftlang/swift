// RUN: %refactor -rename -dump-text -source-filename %s -pos=%(line+2):29 -new-name=renamed | %FileCheck %s --check-prefix=OPTIONAL
// OPTIONAL: shorthand_shadow.swift [[# @LINE+1]]:29 -> [[# @LINE+1]]:32
func renameShorthandBinding(opt: Int?) {
  // RUN: %refactor -rename -dump-text -source-filename %s -pos=%(line+2):10 -new-name=renamed | %FileCheck %s --check-prefix=OPTIONAL
  // OPTIONAL: shorthand_shadow.swift [[# @LINE+1]]:10 -> [[# @LINE+1]]:13
  if let opt {
    // RUN: %refactor -rename -dump-text -source-filename %s -pos=%(line+2):9 -new-name=renamed | %FileCheck %s --check-prefix=OPTIONAL
    // OPTIONAL: shorthand_shadow.swift [[# @LINE+1]]:9 -> [[# @LINE+1]]:12
    _ = opt
  }
}
