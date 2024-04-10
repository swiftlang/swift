// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/subclass_instance_start_adjustment
// RUN: %target-codesign %t/subclass_instance_start_adjustment
// RUN: %target-run %t/subclass_instance_start_adjustment | %FileCheck %s

// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Make sure we get the InstanceStart adjustment right for concrete subclasses
// of generic classes with different sizes, and especially with an unaligned
// size.

class GenericSuperclassWithAlignedSize<T> {
  var field = 42
}

class SubclassOfGenericSuperclassWithAlignedSize: GenericSuperclassWithAlignedSize<Int> {
  var subfield = 43
}

do {
  let obj = SubclassOfGenericSuperclassWithAlignedSize()
  print(obj, obj.field, obj.subfield)
  // CHECK: SubclassOfGenericSuperclassWithAlignedSize 42 43
}

class GenericSuperclassWithMisalignedSize<T> {
  var field = true
}

class SubclassOfGenericSuperclassWithMisalignedSize: GenericSuperclassWithMisalignedSize<Int> {
  var subfield = 44
}

do {
  let obj = SubclassOfGenericSuperclassWithMisalignedSize()
  print(obj, obj.field, obj.subfield)
  // CHECK: SubclassOfGenericSuperclassWithMisalignedSize true 44
}

#if canImport(Foundation)
import Foundation

class GenericSuperclassWithURLField<T> {
  var field: URL?
}

class SubclassOfGenericSuperclassWithURLField: GenericSuperclassWithURLField<Int> {
  var subfield = 45
}

do {
  let obj = SubclassOfGenericSuperclassWithURLField()
  print(obj, obj.field as Any, obj.subfield)
  // CHECK: SubclassOfGenericSuperclassWithURLField nil 45
}
#else
// Simulate the expected output.
print("SubclassOfGenericSuperclassWithURLField nil 45")
#endif
