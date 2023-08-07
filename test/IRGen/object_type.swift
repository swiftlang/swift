// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O %s -o %t/a.out
// RUN: %target-build-swift -O %s -emit-ir | %FileCheck --check-prefix=CHECK-IR %s
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

// Check if the runtime function swift_getObjectType is not readnone and
// therefore not re-scheduled with release-calls, which would lead to a crash
// in this example.

public protocol Proto: class {
   static func printit()
}

public final class ConformingClass : Proto {
   public static func printit() { print("okay") }
}

public final class Creator {
   @inline(never)
   public init() {}

   @inline(never)
   public func createIt() -> Proto {
      return ConformingClass ()
   }
}

func work() {
  let myProtocolType: Proto.Type = type(of: Creator().createIt())
  myProtocolType.printit()
}

// CHECK-IR: call {{.*}} @swift_getObjectType({{.*}}) #[[M:[0-9]+]]
// CHECK-IR: declare {{.*}} @swift_getObjectType{{.*}} local_unnamed_addr #[[N:[0-9]+]]
// CHECK-IR: attributes #[[N]] = { mustprogress nofree nounwind willreturn memory(read) }
// CHECK-IR: attributes #[[M]] = { nounwind willreturn memory(read) }

// CHECK: okay
work()
