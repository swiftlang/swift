// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -verify -O -primary-file %s %S/Inputs/lazy_properties_multi_file_2.swift -c -o %t/lazy_properties_multi_file.o

class MyClass {
    var myProperty = MyGenericStruct<Int>()
}
