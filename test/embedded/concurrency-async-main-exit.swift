// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name main %t/main.swift -emit-silgen | %FileCheck %s
// RUN: %if OS=macosx && CODEGENERATOR=ARM && embedded_stdlib_cross_compiling %{ %target-swift-frontend -target armv7em-none-none-eabi -enable-experimental-feature Embedded -parse-as-library -module-name main %t/main.swift -emit-silgen | %FileCheck %s %}
// RUN: %if OS=wasip1 %{ %target-swift-frontend -enable-experimental-feature Embedded -module-name main %t/toplevel.swift -emit-silgen | %FileCheck %s %}
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name main %t/main.swift -emit-ir | %FileCheck %s --check-prefix=IR

// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx || OS=wasip1

// CHECK-NOT: asmname "exit"
// CHECK-LABEL: sil private [ossa] @async_Main :
// CHECK-NOT: asmname "exit"
// CHECK:         function_ref @$e4main11_swift_exitys5NeverOs5Int32VFTo :
// CHECK-NOT: asmname "exit"
// CHECK:       sil hidden_external [asmname "_swift_exit"] @$e4main11_swift_exitys5NeverOs5Int32VFTo : $@convention(c) (Int32) -> Never
// CHECK-NOT: asmname "exit"

// IR-NOT: @exit(
// IR: declare {{.*}}void @_swift_exit(i32
// IR-NOT: @exit(

//--- main.swift
import _Concurrency

@main
struct Main {
  static func main() async {
    await work()
  }
}

func work() async {}

//--- toplevel.swift
import _Concurrency

func work() async {}

await work()
