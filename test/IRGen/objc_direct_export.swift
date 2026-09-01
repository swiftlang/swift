// REQUIRES: objc_interop

// RUN: %target-swift-frontend -emit-ir -package-name objc_direct_pkg \
// RUN:   -Xcc -fobjc-direct-precondition-thunk %s \
// RUN:   | %FileCheck %s --implicit-check-not='-[_TtC' \
// RUN:     --implicit-check-not='L_selector_data(bar)' \
// RUN:     --implicit-check-not='L_selector_data(initWithValue:)'
//
// RUN: %target-swift-frontend -O -emit-ir -package-name objc_direct_pkg \
// RUN:   -Xcc -fobjc-direct-precondition-thunk %s | %FileCheck %s -check-prefix=OPT

import Foundation

@objc
class Foo: NSObject {
  // The direct entry point uses the C/ObjC convention, not swiftcc, and takes
  // self only -- a direct method has no selector, so there is no _cmd.
  // CHECK-DAG: define hidden void @"-[Foo bar]D"(ptr %0)
  @objcDirect final func bar() {}

  // CHECK-DAG: define hidden void @"+[Foo classMethod]D"(ptr %0)
  @objcDirect static func classMethod() {}

  // CHECK-DAG: define hidden i64 @"-[Foo aPlusBWithInt:b:]D"(ptr %0, i64 %1, i64 %2)
  @objcDirect final func aPlusB(int: Int, b: Int) -> Int { return int + b }

  // A non-direct @objc method keeps its selector-based thunk, which does take
  // _cmd. This is the contrast for the two --implicit-check-not selector
  // assertions on the RUN line above.
  // CHECK-DAG: define internal void @"$s{{.*}}12normalMethodyyFTo"(ptr %0, ptr %1)
  // CHECK-DAG: @"\01L_selector_data(normalMethod)"
  @objc final func normalMethod() {}
}

// Overloaded initializers mangle by selector, so each gets its own symbol.
@objc
class Inits: NSObject {
  // CHECK-DAG: define hidden ptr @"-[Inits initWithValue:]D"(ptr %0, i64 %1)
  @objcDirect init(value: Int) { super.init() }

  // CHECK-DAG: define hidden ptr @"-[Inits initWithName:]D"(ptr %0, ptr %1)
  @objcDirect init(name: String) { super.init() }

  // CHECK-DAG: define hidden ptr @"-[Inits initWithX:y:]D"(ptr %0, i64 %1, i64 %2)
  @objcDirect init(x: Int, y: Int) { super.init() }
}

// The class-name segment is the printed @interface identifier, not the mangled
// runtime name -- guarded by --implicit-check-not='-[_TtC' above. A clang caller
// references -[PlainClass ...]D, so emitting -[_TtC...]D would fail to link.
@objc public class PlainClass: NSObject {
  // CHECK-DAG: define i64 @"-[PlainClass returnOne]D"(ptr %0)
  @objcDirect public final func returnOne() -> Int { return 1 }

  // A method-level @objc(name) renames only the selector segment.
  // CHECK-DAG: define void @"-[PlainClass renamedSelector]D"(ptr %0)
  @objc(renamedSelector) @objcDirect public final func originalName() {}
}

@objc(CustomName) public class Renamed: NSObject {
  // CHECK-DAG: define void @"-[CustomName ping]D"(ptr %0)
  @objcDirect public final func ping() {}
}

// throws bridges to the ObjC NSError** convention: a trailing error pointer and
// still no _cmd.
@objc public enum DirectError: Int, Error { case negative }

@objc public class Throwing: NSObject {
  // CHECK-DAG: define ptr @"-[Throwing throwableWithCond:error:]D"(ptr %0, i64 %1, ptr %2)
  @objcDirect
  public final func throwable(cond: Int) throws -> Throwing {
    if cond < 0 { throw DirectError.negative }
    return self
  }
}

// Visibility follows the context-capped effective access, and non-public direct
// methods must stay out of @llvm.used so they remain DCE-eligible. Forcing the
// linkage on the LinkInfo before createFunction() is what makes this hold:
// markGlobalAsUsedBasedOnLinkage() would otherwise pin External + Default.
@objc public class Visibility: NSObject {
  // CHECK-DAG: define void @"-[Visibility publicDirect]D"(ptr %0)
  @objcDirect public  final func publicDirect()   {}
  // CHECK-DAG: define hidden void @"-[Visibility internalDirect]D"(ptr %0)
  @objcDirect         final func internalDirect() {}
  // CHECK-DAG: define hidden void @"-[Visibility packageDirect]D"(ptr %0)
  @objcDirect package final func packageDirect()  {}
}

// CHECK-NOT: @llvm.used = {{.*}}"-[Visibility internalDirect]D"
// CHECK-NOT: @llvm.used = {{.*}}"-[Visibility packageDirect]D"

// At -O neither used array pins the non-public direct symbols. This also holds
// if SIL dead-function elimination drops them entirely.
// OPT: define void @"-[Visibility publicDirect]D"(
// OPT-NOT: @llvm.used = {{.*}}"-[Visibility internalDirect]D"
// OPT-NOT: @llvm.used = {{.*}}"-[Visibility packageDirect]D"
// OPT-NOT: @llvm.compiler.used = {{.*}}"-[Visibility internalDirect]D"
// OPT-NOT: @llvm.compiler.used = {{.*}}"-[Visibility packageDirect]D"
