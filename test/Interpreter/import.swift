// RUN: %swift -i -I=%S/Inputs %s | FileCheck %s

// Make sure we actually IRGen this.
import implementation

// CHECK: {{^}}1 2 3 4 5{{$}}
implementation.countToFive()
