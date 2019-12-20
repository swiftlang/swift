// RUN: %empty-directory(%t.dir)
// RUN: echo "let foo: Int = goo" > %t.dir/real.swift
// RUN: %{python} %S/../../Inputs/symlink.py %t.dir/real.swift %t.dir/linked.swift
// RUN: %sourcekitd-test -req=sema %t.dir/linked.swift -- %t.dir/real.swift | %sed_clean > %t.link.response
// RUN: diff -u %s.response %t.link.response
// RUN: %sourcekitd-test -req=sema %t.dir/real.swift -- %t.dir/linked.swift | %sed_clean > %t.real.response
// RUN: diff -u %s.response %t.real.response
