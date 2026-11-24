// RUN: %swift_frontend_plain -target x86_64-unknown-windows-msvc -primary-file %s -parse-as-library -parse-stdlib -nostdimport -module-name M -emit-ir -Xcc -static-libclosure -o - | %FileCheck %s
// RUN: %swift_frontend_plain -target x86_64-unknown-windows-msvc -primary-file %s -parse-as-library -parse-stdlib -nostdimport -module-name M -emit-ir -o - | %FileCheck %s --check-prefix=DLLIMPORT

// A block without captures is a global block.
public let block: @convention(block) () -> () = {
}

func use(_ x: Builtin.Int64) {}

// A block with captures is formed on the stack and copied.
public func makeBlock(_ x: Builtin.Int64) -> @convention(block) () -> () {
  return { use(x) }
}

// CHECK-NOT: external dllimport global
// CHECK-DAG: @_NSConcreteGlobalBlock = external global
// CHECK-DAG: @block = private constant {{.*}} @_NSConcreteGlobalBlock
// CHECK-DAG: @_NSConcreteStackBlock = external global
// CHECK-NOT: declare dllimport ptr @_Block_copy(ptr)
// CHECK: declare ptr @_Block_copy(ptr)

// A dllimport'ed isa can't be referenced from a constant global block, so
// without -static-libclosure, all blocks are formed on the stack.
// DLLIMPORT-NOT: _NSConcreteGlobalBlock
// DLLIMPORT: @_NSConcreteStackBlock = external dllimport global
// DLLIMPORT-NOT: _NSConcreteGlobalBlock
// DLLIMPORT: declare dllimport ptr @_Block_copy(ptr)
