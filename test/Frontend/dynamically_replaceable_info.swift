// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c %s %S/Inputs/dynamically_replaceable_info2.swift -module-name A -enable-implicit-dynamic -dynamically-replaceable-info-path %t -c -o %t.o
// RUN: cat %t/Module.swiftrinfo | %FileCheck %s

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -primary-file %s %S/Inputs/dynamically_replaceable_info2.swift -module-name A -enable-implicit-dynamic -dynamically-replaceable-info-path %t -c -o %t.o
// RUN: %target-swift-frontend -c %s -primary-file %S/Inputs/dynamically_replaceable_info2.swift -module-name A -enable-implicit-dynamic -dynamically-replaceable-info-path %t -c -o %t.o
// RUN: cat %t/dynamically_replaceable_info.swift.swiftrinfo %t/dynamically_replaceable_info2.swift.swiftrinfo | %FileCheck %s

struct OuterStruct {
  struct InnerStruct {
    func barFoo() {
    }
  }
  func foobar() {
  }

  func абв() {
  }

  var x : Int {
    get {
      return 0
    }
    set { }
  }

  func get() -> Int {
    return 0
  }

  func get() -> Double {
    return 0.0
  }
}

// CHECK: name:            A
// CHECK: formatVersion:   1
// CHECK: files:
// CHECK:   - name:            '{{.*}}/test/Frontend/dynamically_replaceable_info.swift'
// CHECK:     decls:
// CHECK:       - name:            'A.(file).OuterStruct.InnerStruct.barFoo()'
// CHECK:         type:            '(OuterStruct.InnerStruct) -> () -> ()'
// CHECK:         startLocLine:    12
// CHECK:         startLocCol:     5
// CHECK:         endLocLine:      13
// CHECK:         endLocCol:       5
// CHECK:       - name:            'A.(file).OuterStruct.foobar()'
// CHECK:         type:            '(OuterStruct) -> () -> ()'
// CHECK:       - name:            "A.(file).OuterStruct.абв()"
// CHECK:         type:            '(OuterStruct) -> () -> ()'
// CHECK:       - name:            'A.(file).OuterStruct.x'
// CHECK:         type:            Int
// CHECK:       - name:            'A.(file).OuterStruct.get()'
// CHECK:         type:            '(OuterStruct) -> () -> Int'
// CHECK:       - name:            'A.(file).OuterStruct.get()'
// CHECK:         type:            '(OuterStruct) -> () -> Double'
// CHECK:   - name:            '{{.*}}/test/Frontend/Inputs/dynamically_replaceable_info2.swift'
// CHECK:     decls:
// CHECK:       - name:            'A.(file).OuterClass.subscript(_:)'
// CHECK:         type:            '(Int) -> Int'
// CHECK:       - name:            'A.(file).OuterClass.method()'
// CHECK:         type:            '(OuterClass) -> () -> ()'
