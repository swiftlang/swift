// RUN: %target-swift-emit-silgen -enable-experimental-feature NoImplicitCopy -enable-library-evolution %s | %FileCheck %s

//////////////////////
// MARK: DeinitTest //
//////////////////////

// CHECK-LABEL: sil [ossa] @$s26moveonly_library_evolution10DeinitTestVfD : $@convention(method) (@in DeinitTest) -> () {
// CHECK: bb0([[ARG:%.*]] : $*DeinitTest):
// CHECK:   drop_deinit [[ARG]]
// CHECK: } // end sil function '$s26moveonly_library_evolution10DeinitTestVfD'
public struct DeinitTest : ~Copyable {
    deinit {
    }
}
