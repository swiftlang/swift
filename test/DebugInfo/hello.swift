// RUN: %swift -target x86_64-apple-darwin14 %s -emit-ir -g -o - | FileCheck %s
// This program is so short that it doesn't refer the swift stdlib.
println("Hello World!")
// CHECK: [ DW_TAG_file_type ]{{.*}}lib/swift/macosx/Swift.swiftmodule
