// RUN: rm -rf %t
// RUN: %target-swiftxx-frontend -DCUSTOM -emit-module -cxx-stdlib-path%S/Inputs/c++ -I %S/Inputs %s -module-name Custom -o %t/Custom.swiftmodule
// RUN: not %target-swiftxx-frontend -emit-ir -I %t -I %S/Inputs %s -o - 2>&1 | %FileCheck --check-prefix=CHECK-STRUCT %s
// RUN: not %target-swiftxx-frontend -emit-ir -I %t -I %S/Inputs -DCLASS %s -o - 2>&1 | %FileCheck --check-prefix=CHECK-CLASS %s

#if CUSTOM

@_implementationOnly import CxxHeader

public struct MyStruct {
    private let s: SimpleStruct = SimpleStruct(x: 0, y: 0)
    public let i: Int = 0

    public init() {
    }
}

public class MyClass {
    private let s: SimpleStruct = SimpleStruct(x: 0, y: 0)
    public let i: Int = 0

    public init() {
    }
}


#else

import Custom

public func m() {
#if CLASS
    let c = MyClass()
#else
    let s = MyStruct()
#endif
}
// CHECK-STRUCT: 'MyStruct' from module 'Custom' cannot be imported because it depends on a custom C++ standard library that is not available in the current compilation
// CHECK-CLASS: 'MyClass' from module 'Custom' cannot be imported because it depends on a custom C++ standard library that is not available in the current compilation


#endif
