// RUN: %target-swift-ide-test -print-usrs -source-filename %s | FileCheck %s -strict-whitespace

// CHECK: [[@LINE+1]]:6 s:O14swift_ide_test11InvalidEnum{{$}}
enum InvalidEnum {
  case
}
