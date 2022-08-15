enum EnumWithUnderlyingValue: String {
    case north, south, east, west
}



func foo(test: EnumWithUnderlyingValue) {
  if test == .north {
    print("north")
  } else if test == .south {
    print("south")
  } else if test == .east {
    print("east")
  }
}


// RUN: %empty-directory(%t.result)

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=8:3 -end-pos=14:4 > %t.result/L8-3.swift
// RUN: %target-swift-frontend-typecheck %t.result/L8-3.swift
// RUN: diff -u %S/Outputs/enum_with_raw_value/L8-3.swift.expected %t.result/L8-3.swift
