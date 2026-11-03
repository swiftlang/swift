// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// RUN: %target-swift-frontend -emit-ir -package-name objc_direct_pkg \
// RUN:   -enable-experimental-feature ObjCDirect %s \
// RUN:   | %FileCheck %s --implicit-check-not='-[_TtC' \
// RUN:     --implicit-check-not='L_selector_data(bar)' \
// RUN:     --implicit-check-not='L_selector_data(initWithValue:)'
//
// RUN: %target-swift-frontend -O -emit-ir -package-name objc_direct_pkg \
// RUN:   -enable-experimental-feature ObjCDirect %s | %FileCheck %s -check-prefix=OPT \
// RUN:     --implicit-check-not='ptr @"-[Visibility internalDirect]D"' \
// RUN:     --implicit-check-not='ptr @"-[Visibility packageDirect]D"'

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

  // Contrast: a non-direct @objc method keeps its thunk, which takes _cmd.
  // CHECK-DAG: define internal void @"$s{{.*}}12normalMethodyyFTo"(ptr %0, ptr %1)
  // CHECK-DAG: @"\01L_selector_data(normalMethod)"
  @objc final func normalMethod() {}
}

// Overloaded initializers mangle by selector, so each gets its own symbol.
@objc
final class Inits: NSObject {
  // CHECK-DAG: define hidden ptr @"-[Inits initWithValue:]D"(ptr %0, i64 %1)
  @objcDirect init(value: Int) { super.init() }

  // CHECK-DAG: define hidden ptr @"-[Inits initWithName:]D"(ptr %0, ptr %1)
  @objcDirect init(name: String) { super.init() }

  // CHECK-DAG: define hidden ptr @"-[Inits initWithX:y:]D"(ptr %0, i64 %1, i64 %2)
  @objcDirect init(x: Int, y: Int) { super.init() }
}

// The class segment is the printed @interface name, not the mangled _TtC name.
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

// Visibility follows effective access, so a public member of an internal class
// stays hidden.
@objc public class Visibility: NSObject {
  // CHECK-DAG: define void @"-[Visibility publicDirect]D"(ptr %0)
  @objcDirect public  final func publicDirect()   {}
  // CHECK-DAG: define hidden void @"-[Visibility internalDirect]D"(ptr %0)
  @objcDirect         final func internalDirect() {}
  // CHECK-DAG: define hidden void @"-[Visibility packageDirect]D"(ptr %0)
  @objcDirect package final func packageDirect()  {}
}

// At -O, non-public direct symbols stay out of llvm.used so they can be stripped.
// Not checked at -Onone, where they show up in llvm.used for an unknown reason.
// OPT: @llvm.used = {{.*}}"-[Visibility publicDirect]D"
// OPT: define void @"-[Visibility publicDirect]D"(
