// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -verify -O -primary-file %s %S/Inputs/property_wrappers_multi_file_2.swift -c -o %t/use.o
// RUN: %target-swift-frontend %s -verify -O -primary-file %S/Inputs/property_wrappers_multi_file_2.swift %s -c -o %t/def.o

final class Subclass: BaseClass {
  override init() {
    super.init()
    $value = 42
  }
}
