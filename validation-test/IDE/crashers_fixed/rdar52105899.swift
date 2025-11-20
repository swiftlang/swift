// RUN: %empty-directory(%t)
// RUN: echo '//DUMMY' > %t/dummy.swift
// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s %t/dummy.swift > /dev/null
// RUN: %target-swift-ide-test -code-completion -code-completion-token=B -source-filename=%s %t/dummy.swift > /dev/null

struct MyStruct {
  let _: Int = #^A^#
}

let _: Int = #^B^#
