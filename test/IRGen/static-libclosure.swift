// RUN: %swift_frontend_plain -target x86_64-unknown-windows-msvc -primary-file %s -parse-as-library -parse-stdlib -nostdimport -module-name M -emit-ir -Xcc -static-libclosure -o - | %FileCheck %s

public let block: @convention(block) () -> () = {
}

// CHECK-NOT: @_NSConcreteStackBlock = external dllimport global
// CHECK-NOT: declare dllimport ptr @_Block_copy(ptr)
// CHECK: @_NSConcreteStackBlock = external global
// CHECK: declare ptr @_Block_copy(ptr)
