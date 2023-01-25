// Local rename starts from the `DeclContext` of the renamed `Decl`. For
// closures that means we have no parents, so check that case specifically.

func renameInClosure() {
  // RUN: %refactor -rename -dump-text -source-filename %s -pos=%(line+2):10 -new-name=renamed | %FileCheck %s
  // CHECK: shorthand_shadow.swift [[# @LINE+1]]:10 -> [[# @LINE+1]]:18
  _ = { (toRename: Int?) in
    // RUN: %refactor -rename -dump-text -source-filename %s -pos=%(line+2):12 -new-name=renamed | %FileCheck %s
    // CHECK: shorthand_shadow.swift [[# @LINE+1]]:12 -> [[# @LINE+1]]:20
    if let toRename {
      // RUN: %refactor -rename -dump-text -source-filename %s -pos=%(line+2):11 -new-name=renamed | %FileCheck %s
      // CHECK: shorthand_shadow.swift [[# @LINE+1]]:11 -> [[# @LINE+1]]:19
      _ = toRename
    }
  }
}
