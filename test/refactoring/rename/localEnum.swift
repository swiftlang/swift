// RUN: %empty-directory(%t.result)

func myFunc() {
  print(LocalEnum.north)

  enum LocalEnum {
    case north
    case south
  }

  print(LocalEnum.north)
}

// RUN: %refactor -rename -source-filename %s -pos=4:19 -new-name east > %t.result/north_ref_before.swift
// RUN: diff -u %S/Outputs/localEnum/myFunc_north.swift.expected %t.result/north_ref_before.swift
// RUN: %refactor -rename -source-filename %s -pos=7:10 -new-name east > %t.result/north_def.swift
// RUN: diff -u %S/Outputs/localEnum/myFunc_north.swift.expected %t.result/north_def.swift
// RUN: %refactor -rename -source-filename %s -pos=11:19 -new-name east > %t.result/north_ref_after.swift
// RUN: diff -u %S/Outputs/localEnum/myFunc_north.swift.expected %t.result/north_ref_after.swift

struct Other {
  enum EnumInStruct {
    case first
    case second
  }

  func foo() {
    print(EnumInStruct.first)
  }
}

// RUN: %refactor -rename -source-filename %s -pos=23:10 -new-name primary > %t.result/first_def.swift
// RUN: diff -u %S/Outputs/localEnum/Other_first.swift.expected %t.result/first_def.swift
// RUN: %refactor -rename -source-filename %s -pos=28:24 -new-name primary > %t.result/first_ref.swift
// RUN: diff -u %S/Outputs/localEnum/Other_first.swift.expected %t.result/first_ref.swift

func nested() {
  struct MyStruct {
    enum NestedEnum {
      case a
      case b
    }
  }

  print(MyStruct.NestedEnum.a)
}

// RUN: %refactor -rename -source-filename %s -pos=40:12 -new-name one > %t.result/a_def.swift
// RUN: diff -u %S/Outputs/localEnum/nested_a.swift.expected %t.result/a_def.swift
// RUN: %refactor -rename -source-filename %s -pos=45:29 -new-name one > %t.result/a_ref.swift
// RUN: diff -u %S/Outputs/localEnum/nested_a.swift.expected %t.result/a_ref.swift
