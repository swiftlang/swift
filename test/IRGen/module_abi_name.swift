// RUN: %target-swift-frontend -module-name=Hello -module-abi-name Goodbye -emit-ir %s | %FileCheck %s

// CHECK-NOT: {{(5|")Hello}}

// CHECK-DAG: [[MODULE_NAME:@.*]] = private constant [8 x i8] c"Goodbye\00"
// CHECK-DAG: @"$s7GoodbyeMXM" = {{.*}} [[MODULE_NAME]]

// CHECK-DAG: @"$s7Goodbye8GreetingCMm" =
public class Greeting { }

// CHECK-NOT: {{(5|")Hello}}
// CHECK: define swiftcc void @"$s7Goodbye8functionyyF"()
@inlinable public func function() { }

func callFunction() {
  function()
}

// CHECK-NOT: {{(5|")Hello}}
