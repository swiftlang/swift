// RUN: %empty-directory(%t.dir)
// RUN: echo "let foo = 0" > %t.dir/real.swift
// RUN: %{python} %S/../../Inputs/symlink.py %t.dir/real.swift %t.dir/linked.swift
// RUN: %sourcekitd-test -req=cursor -pos=1:5 %t.dir/linked.swift -- %t.dir/real.swift | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=1:5 %t.dir/real.swift -- %t.dir/linked.swift | %FileCheck %s

// CHECK: source.lang.swift.decl.var.global (1:5-1:8)
// CHECK: foo
