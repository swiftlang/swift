// RUN: %target-swift-emit-ir -parse-as-library -module-name main %s -enable-experimental-feature Embedded -wmo -Onone | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-cpu
// RUN: %target-swift-emit-ir -parse-as-library -module-name main %s -enable-experimental-feature Embedded -wmo -O | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-cpu

// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx

// Blocks that don't capture any context are emitted as global blocks. In
// Embedded Swift, this includes closures in generic functions, which only
// become context-free once they are specialized.
//
// On arm64e the fields of a block are signed like Clang signs them, so they
// refer to `.ptrauth` constants.

@_silgen_name("takesBlock")
func takesBlock(_ b: @escaping @convention(block) () -> Void)

func hello() {}

@_silgen_name("use")
func use(_ x: Int)

// CHECK-DAG: @_NSConcreteGlobalBlock = external global %objc_class
// CHECK-DAG: [[NONGENERIC_BLOCK:@block[.0-9]*]] = private constant { %objc_block, %swift.function } { %objc_block { ptr [[NONGENERIC_ISA:@_NSConcreteGlobalBlock(\.ptrauth[.0-9]*)?]], i32 1342177280, {{.*}} }, %swift.function { ptr @"$e4main10nonGenericyyFyycfU_{{(\.ptrauth)?}}", ptr null } }
// CHECK-DAG: [[GENERIC_BLOCK:@block[.0-9]*]] = private constant { %objc_block, %swift.function } { %objc_block { ptr [[GENERIC_ISA:@_NSConcreteGlobalBlock(\.ptrauth[.0-9]*)?]], i32 1342177280, {{.*}} }, %swift.function { ptr @"$e4main11makeGenericyyxmlFyycfU_Si_Tg5{{(\.ptrauth)?}}", ptr null } }

// The isa of a global block is signed with the isa schema (DA key, address
// diversity, discriminator 0x6ae1), just like that of a stack block.
// CHECK-arm64e-DAG: [[NONGENERIC_ISA]] = private constant { ptr, i32, i64, i64 } { ptr @_NSConcreteGlobalBlock, i32 2, i64 ptrtoint (ptr [[NONGENERIC_BLOCK]] to i64), i64 27361 }, section "llvm.ptrauth"
// CHECK-arm64e-DAG: [[GENERIC_ISA]] = private constant { ptr, i32, i64, i64 } { ptr @_NSConcreteGlobalBlock, i32 2, i64 ptrtoint (ptr [[GENERIC_BLOCK]] to i64), i64 27361 }, section "llvm.ptrauth"

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
// CHECK:         {{store ptr @_NSConcreteStackBlock|call i64 @llvm.ptrauth.sign\(i64 ptrtoint \(ptr @_NSConcreteStackBlock to i64\), i32 2}}
// CHECK:         call ptr @_Block_copy
// CHECK:       }
public func capturing(x: Int) {
  takesBlock { use(x) }
}
