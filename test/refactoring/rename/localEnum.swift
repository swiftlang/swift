func boop() {
  enum LocalEnum {
    case north
    case south
  }

  print(LocalEnum.north)
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -rename -source-filename %s -pos=3:10 -new-name east > %t.result/north_def.swift
// RUN: diff -u %S/Outputs/localEnum/north.swift.expected %t.result/north_def.swift
// RUN: %refactor -rename -source-filename %s -pos=7:19 -new-name east > %t.result/north_ref.swift
// RUN: diff -u %S/Outputs/localEnum/north.swift.expected %t.result/north_ref.swift
