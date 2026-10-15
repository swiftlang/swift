// Tests that under -enable-llvm-wme, protocol witness table calls to protocols
// defined by other modules are using thunks (instead of direct wtable loads).

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -enable-llvm-wme -parse-as-library %s -DLIBRARY -module-name Library -emit-module -o %t/Library.swiftmodule
// RUN: %target-build-swift -Xfrontend -enable-llvm-wme -parse-as-library %s -DCLIENT -module-name Main -I%t -emit-ir -o - | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -enable-llvm-wme -parse-as-library %s -DIMPLEMENTATION -module-name Implementation -I%t -O -emit-ir -o - | %FileCheck %s --check-prefixes=PRIVATE-CONFORMANCE,PUBLIC-VISIBILITY
// RUN: %target-build-swift -Xfrontend -enable-llvm-wme -Xfrontend -internalize-at-link -parse-as-library %s -DIMPLEMENTATION -module-name Implementation -I%t -O -emit-ir -o - | %FileCheck %s --check-prefixes=PRIVATE-CONFORMANCE,LINKAGE-VISIBILITY

#if LIBRARY

public protocol MyLibraryProtocol {
  func library_req()
}

#endif

#if IMPLEMENTATION

import Library

private struct PrivateConformer: MyLibraryProtocol {
  func library_req() {}
}

public func makePrivateConformer() -> any MyLibraryProtocol {
  PrivateConformer()
}

// A private conformance can escape its module as an existential. The protocol
// dispatch thunk lives in Library, so pre-link VFE in Implementation cannot
// see the call and must preserve the witness until link time.
// PRIVATE-CONFORMANCE: = internal constant [2 x ptr] [ptr {{.*}}, ptr @"{{.*}}PrivateConformer{{.*}}library_reqyyFTW"], {{.*}}!vcall_visibility ![[VIS:[0-9]+]]
// PUBLIC-VISIBILITY: ![[VIS]] = !{i64 0,
// LINKAGE-VISIBILITY: ![[VIS]] = !{i64 1,

#endif

#if CLIENT

import Library

public protocol MyLocalProtocol {
  func local_req()
}

extension MyLocalProtocol {
  func func1() {
    // CHECK: define hidden swiftcc void @"$s4Main15MyLocalProtocolPAAE5func1yyF"
    self.local_req()
    // CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr {{.*}}, i32 1
    // CHECK: call { ptr, i1 } @llvm.type.checked.load(ptr [[SLOT]], i32 0, metadata !"$s4Main15MyLocalProtocolP9local_reqyyFTq")
    // CHECK: ret void
  }
}

extension MyLibraryProtocol {
  func func2() {
    // CHECK: define hidden swiftcc void @"$s7Library02MyA8ProtocolP4MainE5func2yyF"
    self.library_req()
    // CHECK: call swiftcc void @"$s7Library02MyA8ProtocolP11library_reqyyFTj"

    // CHECK-NOT: @llvm.type.checked.load
    // CHECK: ret void
  }
}

#endif
