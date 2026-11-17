// RUN: %target-swift-emit-ir -I %S/Inputs -enable-experimental-cxx-interop %s | %FileCheck %s
// RUN: %if swift_feature_OnceFunctions %{ %target-swift-emit-ir -I %S/Inputs -enable-experimental-cxx-interop %s -enable-experimental-feature OnceFunctions -D EXACTLY_ONCE | %FileCheck %s --check-prefix=STATIC-INIT %}
// RUN: %if objc_interop && OS=macosx %{ %target-swift-emit-ir -I %S/Inputs -enable-experimental-cxx-interop %s -target %target-cpu-apple-macosx11.0 -D OBJC_ACTOR | %FileCheck %s --check-prefix=STATIC-INIT %}
// RUN: %if objc_interop && OS=macosx %{ %target-swift-emit-ir -I %S/Inputs -enable-experimental-cxx-interop %s -D EAGER_CLASS_INIT | %FileCheck %s --check-prefix=STATIC-INIT %}

import InlineStaticMemberVar

public func readStaticMember() -> CInt {
  return WithInlineStaticMember.staticMember
}

// CHECK: @{{_ZN22WithInlineStaticMember12staticMemberE|"\?staticMember@WithInlineStaticMember@@2HA"}} = linkonce_odr {{(dso_local )?}}global i32 12, {{(comdat, )?}}align 4

// CHECK: define {{(protected |dllexport )?}}swiftcc i32 @"$s4main16readStaticMembers5Int32VyF"()
// CHECK: [[VALUE:%.*]] = load i32, ptr @{{_ZN22WithInlineStaticMember12staticMemberE|"\?staticMember@WithInlineStaticMember@@2HA"}}, align 4
// CHECK: ret i32 [[VALUE]]

public func writeStaticMember(_ c: CInt) {
  WithInlineStaticMember.staticMember = c
}

// CHECK: define {{(protected |dllexport )?}}swiftcc void @"$s4main17writeStaticMemberyys5Int32VF"(i32 %0)
// CHECK: store i32 %0, ptr @{{_ZN22WithInlineStaticMember12staticMemberE|"\?staticMember@WithInlineStaticMember@@2HA"}}, align 4

func modifyInout(_ c: inout CInt) {
  c = 42
}

public func passingVarAsInout() {
  modifyInout(&WithInlineStaticMember.staticMember)
}

// CHECK: define {{(protected |dllexport )?}}swiftcc void @"$s4main17passingVarAsInoutyyF"()
// CHECK: call swiftcc void @"$s4main11modifyInoutyys5Int32VzF"(ptr noalias align 4 captures(none) dereferenceable(4) @{{_ZN22WithInlineStaticMember12staticMemberE|"\?staticMember@WithInlineStaticMember@@2HA"}})

// The dynamic initialization of `staticMemberInitializedAtRuntime` is a C++
// global constructor, and the compiler's own static initializers are added to
// the same list of global constructors.

public func readStaticMemberInitializedAtRuntime() -> CInt {
  return WithInlineStaticMember.staticMemberInitializedAtRuntime
}

#if EXACTLY_ONCE
// A captureless exactly-once closure uses a context that is initialized at load
// time.
func callExactlyOnce(_ body: @once () -> CInt) -> CInt {
  body()
}

public func readInExactlyOnceClosure() -> CInt {
  callExactlyOnce { WithInlineStaticMember.staticMemberInitializedAtRuntime }
}
#endif

#if OBJC_ACTOR
import Foundation

// Before macOS 12, the superclass of an @objc actor is swizzled at load time.
@objc actor ObjCActor {}
#endif

#if EAGER_CLASS_INIT
import Foundation

class BaseWithCoding: NSObject, NSCoding {
  required init(coder: NSCoder) { fatalError() }
  func encode(with coder: NSCoder) { fatalError() }
}

class Generic<T>: BaseWithCoding {}

// The metadata of a class with a generic ancestor and a custom Objective-C
// name is initialized at load time.
@objc(CustomName)
class GenericAncestryWithCustomName: Generic<Int> {}
#endif

// STATIC-INIT: @llvm.global_ctors = appending global [2 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @{{.*}}, ptr {{.*}} }, { i32, ptr, ptr } { i32 60000, ptr @{{.*}}, ptr null }]
// STATIC-INIT-NOT: @llvm.global_ctors.
