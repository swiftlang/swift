// RUN: %target-swift-frontend -module-name=Hello -module-abi-name Goodbye -emit-ir %s | %FileCheck %s

// CHECK-NOT: {{(5|")Hello}}

// CHECK: [[MODULE_NAME:@.*]] = private constant [8 x i8] c"Goodbye\00"
// CHECK: @"$s7GoodbyeMXM" = {{.*}} [[MODULE_NAME]]
// CHECK: @"$s7Goodbye8GreetingCMn" =
public class Greeting { }



// CHECK-NOT: {{(5|")Hello}}
// CHECK: define {{.*}} @"$s7Goodbye8functionyyF"
@inlinable public func function() { }

func callFunction() {
  function()
}

// CHECK-NOT: {{(5|")Hello}}
