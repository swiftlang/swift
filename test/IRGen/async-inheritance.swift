// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(L)) -module-name L -emit-module -emit-module-path %t/L.swiftmodule %s -DL
// RUN: %target-build-swift -I%t -L%t -lL -parse-as-library %s -module-name E -o %t/E %target-rpath(%t)
// RUN: %target-codesign %t/E
// RUN: %target-run %t/E %t/%target-library-name(L) | %FileCheck %s

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
