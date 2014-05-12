// RUN: %swift -i -I=%S/Inputs -enable-source-import %s | FileCheck %s
// REQUIRES: swift_interpreter

// Make sure we actually IRGen this.
import implementation

println("Hello")

// CHECK: {{^}}1 2 3 4 5{{$}}
implementation.countToFive()

// Run it again to make sure we don't IRGen again.
// CHECK: {{^}}1 2 3 4 5{{$}}
implementation.countToFive()
