// REQUIRES: objc_interop
// RUN: %complete-test -hide-low-priority=1 -tok=TOP_LEVEL_0 %s -- -I %S/Inputs > %t.on
// RUN: %complete-test -hide-low-priority=0 -tok=TOP_LEVEL_0 %s -- -I %S/Inputs > %t.off

// RUN: %FileCheck %s -check-prefix=HIDE < %t.on
// RUN: %FileCheck %s -check-prefix=NOHIDE < %t.off

// RUN: %complete-test -hide-by-name=1 -hide-low-priority=1 -tok=TOP_LEVEL_TYPE_0 %s -- -I %S/Inputs | %FileCheck %s -check-prefix=TYPES-HIDENAME
// RUN: %complete-test -hide-by-name=0 -hide-low-priority=1 -tok=TOP_LEVEL_TYPE_0 %s -- -I %S/Inputs | %FileCheck %s -check-prefix=TYPES-NOHIDENAME
import PopularAPI

let x = 1

func test(y: Int) {
  let z = 2
  #^TOP_LEVEL_0^#
}

// HIDE: y
// HIDE: z
// HIDE: x
// HIDE-NOT: ModuleColor
// HIDE-NOT: Int
// HIDE-NOT: import

// NOHIDE: func
// NOHIDE: y
// NOHIDE: z
// NOHIDE: x
// NOHIDE: import
// NOHIDE: ModuleColor
// NOHIDE: Int

func testType() {
  let x: #^TOP_LEVEL_TYPE_0^#
}
// TYPES-HIDENAME: ModuleColor
// TYPES-HIDENAME-NOT: BY_NAME_BAD
// TYPES-HIDENAME-NOT: byNameBad
// TYPES-HIDENAME-NOT: bynamebad
// TYPES-HIDENAME-NOT: by_name_bad
// TYPES-HIDENAME: {{^}}Int{{$}}

// TYPES-NOHIDENAME: ModuleColor
// TYPES-NOHIDENAME: BY_NAME_BAD
// TYPES-NOHIDENAME: by_name_bad
// TYPES-NOHIDENAME: byNameBad
// TYPES-NOHIDENAME: bynamebad
// TYPES-NOHIDENAME: {{^}}Int{{$}}
