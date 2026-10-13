// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir %s -parse-as-library -module-name main -enable-experimental-feature Embedded -wmo -o %t/explicit.ll
// RUN: %FileCheck %s < %t/explicit.ll
// RUN: %FileCheck %s --check-prefix=NO-REPLACEMENT < %t/explicit.ll
// RUN: %target-swift-frontend -emit-ir %s -parse-as-library -module-name main -enable-experimental-feature Embedded -wmo -enable-implicit-dynamic -o %t/implicit.ll
// RUN: %FileCheck %s < %t/implicit.ll
// RUN: %FileCheck %s --check-prefix=NO-REPLACEMENT < %t/implicit.ll

// REQUIRES: swift_feature_Embedded

// Embedded Swift does not support dynamic replacement, so 'dynamic' and
// '@_dynamicReplacement' are ignored rather than producing references to the
// dynamic replacement runtime entry points.

// NO-REPLACEMENT-NOT: swift_getFunctionReplacement
// NO-REPLACEMENT-NOT: swift_getOrigOfReplaceable
// NO-REPLACEMENT-NOT: {{\$e.*TX}}

public dynamic func dynamicFunc() -> Int { 1 }

public struct S {
  public dynamic var stored: Int = 0
  public dynamic var computed: Int { 2 }
  public dynamic subscript(i: Int) -> Int { i }
  public dynamic init() { }
}

public class C {
  public dynamic func method() -> Int { 3 }
  public init() { }
}

@_dynamicReplacement(for: dynamicFunc())
func replacementFunc() -> Int { 4 }

// The replacement is an ordinary function, and calls to the original go
// straight to it.
// CHECK-LABEL: define {{.*}}@"$e4main3useSiyF"()
// CHECK: call {{.*}}@"$e4main11dynamicFuncSiyF"()
public func use() -> Int {
  dynamicFunc() + S().computed + S()[0] + C().method() + replacementFunc()
}
