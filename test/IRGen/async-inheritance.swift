// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(L)) -Xfrontend -disable-availability-checking -module-name L -emit-module -emit-module-path %t/L.swiftmodule %s -DL
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -I%t -L%t -lL -parse-as-library %s -module-name E -o %t/E %target-rpath(%t)
// RUN: %target-codesign %t/E
// RUN: %target-codesign %t/%target-library-name(L)
// RUN: %target-run %t/E %t/%target-library-name(L) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: executable_test

// UNSUPPORTED: back_deployment_runtime

#if L
open class C {
  public init() {}
  open func f() async {
    print("\(#function)")
  }
}
#else
import L
class D: C {
}

@main
struct S {
  public static func main() async {
    await D().f()
  }
}
#endif

// CHECK: f()
