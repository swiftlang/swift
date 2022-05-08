enum Foo {
  case first(associated: Int)
  case second
}

func test() {
  let _ = Foo.first(associated: 1)
  let _ = Foo.first
  let _ = Foo.second
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -rename -source-filename %s -pos=2:8 -new-name 'primary(with:)' > %t.result/first_def.swift
// RUN: diff -u %S/Outputs/enum/first.swift.expected %t.result/first_def.swift
// RUN: %refactor -rename -source-filename %s -pos=7:15 -new-name 'primary(with:)' > %t.result/first_ref.swift
// RUN: diff -u %S/Outputs/enum/first.swift.expected %t.result/first_ref.swift
// RUN: %refactor -rename -source-filename %s -pos=7:21 -new-name 'primary(with:)' > %t.result/first_ref_assoc.swift
// RUN: diff -u %S/Outputs/enum/first.swift.expected %t.result/first_ref_assoc.swift
// RUN: %refactor -rename -source-filename %s -pos=8:15 -new-name 'primary(with:)' > %t.result/first_ref_base.swift
// RUN: diff -u %S/Outputs/enum/first.swift.expected %t.result/first_ref_assoc.swift

// RUN: %refactor -rename -source-filename %s -pos=3:8 -new-name 'secondary' > %t.result/second_def.swift
// RUN: diff -u %S/Outputs/enum/second.swift.expected %t.result/second_def.swift
// RUN: %refactor -rename -source-filename %s -pos=9:15 -new-name 'secondary' > %t.result/second_ref.swift
// RUN: diff -u %S/Outputs/enum/second.swift.expected %t.result/second_ref.swift
