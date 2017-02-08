// RUN: %target-swift-ide-test -new-mangling-for-tests -print-usrs -source-filename %s | %FileCheck %s -strict-whitespace

// CHECK: [[@LINE+1]]:6 s:14swift_ide_test11InvalidEnumO{{$}}
enum InvalidEnum {
  case
}
