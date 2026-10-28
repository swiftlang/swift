// RUN: %target-swift-emit-ir -parse-as-library -module-name main %s -enable-experimental-feature Embedded -wmo -Onone | %FileCheck %s
// RUN: %target-swift-emit-ir -parse-as-library -module-name main %s -enable-experimental-feature Embedded -wmo -O | %FileCheck %s

// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx

// Blocks that don't capture any context are emitted as global blocks. In
// Embedded Swift, this includes closures in generic functions, which only
// become context-free once they are specialized.

@_silgen_name("takesBlock")
func takesBlock(_ b: @escaping @convention(block) () -> Void)

func hello() {}

@_silgen_name("use")
func use(_ x: Int)

// CHECK-DAG: @_NSConcreteGlobalBlock = external global %objc_class
// CHECK-DAG: [[NONGENERIC_BLOCK:@block[.0-9]*]] = private constant { %objc_block, %swift.function } { %objc_block { ptr @_NSConcreteGlobalBlock, i32 1342177280, {{.*}} }, %swift.function { ptr @"$e4main10nonGenericyyFyycfU_", ptr null } }
// CHECK-DAG: [[GENERIC_BLOCK:@block[.0-9]*]] = private constant { %objc_block, %swift.function } { %objc_block { ptr @_NSConcreteGlobalBlock, i32 1342177280, {{.*}} }, %swift.function { ptr @"$e4main11makeGenericyyxmlFyycfU_Si_Tg5", ptr null } }

// CHECK-LABEL: define {{.*}}@"$e4main10nonGenericyyF"()
// CHECK-NOT:     _Block_copy
// CHECK:         call {{.*}}@takesBlock(ptr {{(nonnull )?}}[[NONGENERIC_BLOCK]])
// CHECK:       }
public func nonGeneric() {
  takesBlock { hello() }
}

@inline(never)
func makeGeneric<T>(_ t: T.Type) {
  takesBlock { _ = T.self }
}

public func useGeneric() {
  makeGeneric(Int.self)
}

// CHECK-LABEL: define {{.*}}@"$e4main11makeGenericyyxmlFSi_T{{t?}}g5"()
// CHECK-NOT:     _Block_copy
// CHECK:         call {{.*}}@takesBlock(ptr {{(nonnull )?}}[[GENERIC_BLOCK]])
// CHECK:       }

// A block with a capture is formed on the stack and copied.
// CHECK-LABEL: define {{.*}}@"$e4main9capturing1xySi_tF"(i64 %0)
// CHECK:         store ptr @_NSConcreteStackBlock
// CHECK:         call ptr @_Block_copy
// CHECK:       }
public func capturing(x: Int) {
  takesBlock { use(x) }
}
