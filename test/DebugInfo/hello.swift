// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

println("Hello World!")
// CHECK: [ DW_TAG_file_type ] [{{.*}}/lib/swift/{{[^/]+(/32)?}}/Swift.swiftmodule]
