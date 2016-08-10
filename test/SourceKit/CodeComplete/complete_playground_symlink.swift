// RUN: rm -rf %t.dir
// RUN: mkdir -p %t.dir/test.playground
// RUN: echo "let foo_blah = 0" > %t.dir/input.swift
// RUN: echo "" >> %t.dir/input.swift
// RUN: ln -s %t.dir/test.playground %t.dir/linked.playground
// RUN: %sourcekitd-test -req=complete -pos=2:1 -text-input %t.dir/input.swift %t.dir/test.playground -- %t.dir/test.playground | %FileCheck %s
// RUN: %sourcekitd-test -req=complete -pos=2:1 -text-input %t.dir/input.swift %t.dir/linked.playground -- %t.dir/linked.playground | %FileCheck %s

// CHECK: key.name: "foo_blah"
