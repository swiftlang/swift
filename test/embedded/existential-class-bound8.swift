// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -c -o %t/MyModule.o %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -c -I %t %t/Main.swift -enable-experimental-feature Embedded -o %t/a.o

// RUN: %llvm-nm -a %t/MyModule.o | %FileCheck --check-prefix=ORIGINBINARY %s
// RUN: %llvm-nm -a %t/a.o | %FileCheck --check-prefix=CLIENTBINARY %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded


// ORIGINBINARY-NOT: e8MyModule0A5ClassCAA08InternalC5BoundAAWP
// CLIENTBINARY: {{(S|V|W)}} {{(_)?}}$e8MyModule0A5ClassCAA08InternalC5BoundAAWP

// BEGIN MyModule.swift

public protocol Base: AnyObject {
    func bar()
}

public protocol ClassBound: Base {
    func foo()
}


public protocol InternalClassBound : AnyObject {
  func foo()
}

public class CaptureExistential : ClassBound {
  let c : InternalClassBound

  init(_ e: InternalClassBound) {
    self.c = e
  }

  public func foo() {
    c.foo()
  }

  public func bar() { print("bar") }
}

public class MyClass : InternalClassBound {
  public init() {}
  public func foo() { print("MyClass.foo()") }
}

class MyGenericClass<T> {
    var typ: String
    init(typ: String) { self.typ = typ }
}
extension MyGenericClass: ClassBound {
    func bar() { print("MyGenericClass<\(typ)>.bar()") }
    func foo() { print("MyGenericClass<\(typ)>.foo()") }
}

public func factory() -> any ClassBound {
    return MyGenericClass<String>(typ: "String")
}


@inline(never)
func genericCreate<T: InternalClassBound>(_ t: T) -> CaptureExistential {
    let exist : InternalClassBound = t
    return CaptureExistential(exist)
}

@inline(never)
public func genericFactory<T: InternalClassBound>(_ t: T) -> CaptureExistential? {
    return genericCreate(t)
}

// BEGIN Main.swift

import MyModule

var arr: [any ClassBound] = [factory(), genericFactory(MyClass())!]
arr[0].foo()
// CHECK: MyGenericClass<String>.foo()
arr[0].foo()
// CHECK: MyGenericClass<String>.bar()
