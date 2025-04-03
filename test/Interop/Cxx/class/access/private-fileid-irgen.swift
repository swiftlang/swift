//--- blessed.swift
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-ir -module-name main %t/blessed.swift -I %S/Inputs -cxx-interoperability-mode=default -Onone | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -module-name main %t/blessed.swift -I %S/Inputs -cxx-interoperability-mode=default -Onone -g | %FileCheck %s

import NonPublic

// These extension methods are just here to make it clear what we are doing to
// each Int32-typed member.
extension Int32 {
    func read() { }
    mutating func write() { }
}

extension MyClass {
    public func extMethod() {
        publMethod()
        privMethod()
    }
// CHECK: define {{.*}}swiftcc void @"{{.*}}extMethod{{.*}}"
// CHECK:   {{invoke|call}} void @{{.*}}publMethod{{.*}}
// CHECK:   {{invoke|call}} void @{{.*}}privMethod{{.*}}

    public mutating func extMutatingMethod() {
        publMutatingMethod()
        privMutatingMethod()
    }
// CHECK: define {{.*}}swiftcc void @"{{.*}}extMutatingMethod{{.*}}"
// CHECK:   {{invoke|call}} void @{{.*}}publMutatingMethod{{.*}}
// CHECK:   {{invoke|call}} void @{{.*}}privMutatingMethod{{.*}}

    public func extVarRead() {
        publVar.read()
        privVar.read()
    }
// CHECK: define {{.*}}swiftcc void @"{{.*}}extVarRead{{.*}}"

    public mutating func extVarWrite() {
        publVar.write()
        privVar.write()
    }
// CHECK: define {{.*}}swiftcc void @"{{.*}}extVarWrite{{.*}}"

    public func extStaticFunc() {
        MyClass.publStaticFunc()
        MyClass.privStaticFunc()
    }
// CHECK: define {{.*}}swiftcc void @"{{.*}}extStaticFunc{{.*}}"
// CHECK:   {{invoke|call}} void @{{.*}}publStaticFunc
// CHECK:   {{invoke|call}} void @{{.*}}privStaticFunc

    public func extStaticVarRead() {
        MyClass.publStaticVar.read()
        MyClass.privStaticVar.read()
    }
// CHECK: define {{.*}}swiftcc void @"{{.*}}extStaticVarRead{{.*}}"

    public func extStaticVarWrite() {
        MyClass.publStaticVar.write()
        MyClass.privStaticVar.write()
    }
// CHECK: define {{.*}}swiftcc void @"{{.*}}extStaticVarWrite{{.*}}"

    public func extTypedef() {
      let u: publTypedef = publTypedefMake()
      publTypedefTake(u)
      let i: privTypedef = privTypedefMake()
      privTypedefTake(i)
    }
// CHECK: define {{.*}}swiftcc void @"{{.*}}extTypedef{{.*}}"
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}publTypedefMake{{.*}}
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}publTypedefTake{{.*}}
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}privTypedefMake{{.*}}
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}privTypedefTake{{.*}}

    public func extStruct() {
      let u: publStruct = publStructMake()
      publStructTake(u)
      let i: privStruct = privStructMake()
      privStructTake(i)
    }
// CHECK: define {{.*}}swiftcc void @"{{.*}}extStruct{{.*}}"
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}publStructMake{{.*}}
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}publStructTake{{.*}}
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}privStructMake{{.*}}
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}privStructTake{{.*}}

    public func extEnum() {
      let u: publEnum = publEnumMake()
      publEnumTake(u)
      let i: privEnum = privEnumMake()
      privEnumTake(i)
    }
// CHECK: define {{.*}}swiftcc void @"{{.*}}extEnum{{.*}}"
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}publEnumMake{{.*}}
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}publEnumTake{{.*}}
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}privEnumMake{{.*}}
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}privEnumTake{{.*}}

    // If we call this extEnumClass, the name gets mangled to something else.
    public func extEnumCls() {
      let u: publEnumClass = publEnumClassMake()
      publEnumClassTake(u)
      let i: privEnumClass = privEnumClassMake()
      privEnumClassTake(i)
    }
// CHECK: define {{.*}}swiftcc void @"{{.*}}extEnumCls{{.*}}"
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}publEnumClassMake{{.*}}
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}publEnumClassTake{{.*}}
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}privEnumClassMake{{.*}}
// CHECK:   {{invoke|call}} {{.*}} @{{.*}}privEnumClassTake{{.*}}
}
