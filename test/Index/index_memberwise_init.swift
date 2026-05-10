// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

struct DefaultedStruct {
  var x = 17
  var y = true
  var z = "hello"
}

// Make sure indexing uses the original arguments when adding references to
// the properties in a memberwise initializer
func useDefaultInits() {
  // CHECK-NOT: s:14swift_ide_test15DefaultedStructV1xSbvp
  // CHECK-NOT: s:14swift_ide_test15DefaultedStructV1zSbvp
  // CHECK: [[@LINE+2]]:23 | instance-property/Swift | y | s:14swift_ide_test15DefaultedStructV1ySbvp | Ref,RelCont
  // CHECK: [[@LINE+1]]:7 | constructor/Swift | init(x:y:z:) | s:14swift_ide_test15DefaultedStructV1x1y1zACSi_SbSStcfc | Ref,Call,RelCall,RelCont | rel: 1
  _ = DefaultedStruct(y: false)
}

