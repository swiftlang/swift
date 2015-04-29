// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

println("Hello World!")
// CHECK: !DIFile(filename: "Swift.swiftmodule",
