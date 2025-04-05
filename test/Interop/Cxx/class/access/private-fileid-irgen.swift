//--- blessed.swift
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-ir -module-name main %t/blessed.swift -I %S/Inputs -cxx-interoperability-mode=default -Onone
// RUN: %target-swift-frontend -emit-ir -module-name main %t/blessed.swift -I %S/Inputs -cxx-interoperability-mode=default -Onone | %FileCheck %s

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
// CHECK:   invoke void @{{.*}}publMethod{{.*}}
// CHECK:   invoke void @{{.*}}privMethod{{.*}}

    public mutating func extMutatingMethod() {
        publMutatingMethod()
        privMutatingMethod()
    }
// CHECK: define {{.*}}swiftcc void @"{{.*}}extMutatingMethod{{.*}}"
// CHECK:   invoke void @{{.*}}publMutatingMethod{{.*}}
// CHECK:   invoke void @{{.*}}privMutatingMethod{{.*}}

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
// CHECK:   invoke void @{{.*}}publStaticFunc
// CHECK:   invoke void @{{.*}}privStaticFunc

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
// CHECK:   %[[uTypedef:.*]] = invoke [[utTypedef:.*]] @{{.*}}publTypedefMake{{.*}}
// CHECK:   invoke void @{{.*}}publTypedefTake{{.*}}({{.*}}, [[utTypedef]] %[[uTypedef]])
// CHECK:   %[[iTypedef:.*]] = invoke [[itTypedef:.*]] @{{.*}}privTypedefMake{{.*}}
// CHECK:   invoke void @{{.*}}privTypedefTake{{.*}}({{.*}}, [[itTypedef]] %[[iTypedef]])

    public func extStruct() {
      let u: publStruct = publStructMake()
      publStructTake(u)
      let i: privStruct = privStructMake()
      privStructTake(i)
    }
// CHECK: define {{.*}}swiftcc void @"{{.*}}extStruct{{.*}}"
// CHECK:   %[[uStruct:.*]] = invoke [[utStruct:.*]] @{{.*}}publStructMake{{.*}}
// CHECK:   invoke void @{{.*}}publStructTake{{.*}}
// CHECK:   %[[iStruct:.*]] = invoke [[itStruct:.*]] @{{.*}}privStructMake{{.*}}
// CHECK:   invoke void @{{.*}}privStructTake{{.*}}

    public func extEnum() {
      let u: publEnum = publEnumMake()
      publEnumTake(u)
      let i: privEnum = privEnumMake()
      privEnumTake(i)
    }
// CHECK: define {{.*}}swiftcc void @"{{.*}}extEnum{{.*}}"
// CHECK:   %[[uEnum:.*]] = invoke [[utEnum:.*]] @{{.*}}publEnumMake{{.*}}
// CHECK:   invoke void @{{.*}}publEnumTake{{.*}}({{.*}}, [[utEnum]] %[[uEnum]])
// CHECK:   %[[iEnum:.*]] = invoke [[itEnum:.*]] @{{.*}}privEnumMake{{.*}}
// CHECK:   invoke void @{{.*}}privEnumTake{{.*}}({{.*}}, [[itEnum]] %[[iEnum]])

    // If we call this extEnumClass, the name gets mangled to something else.
    public func extEnumCls() {
      let u: publEnumClass = publEnumClassMake()
      publEnumClassTake(u)
      let i: privEnumClass = privEnumClassMake()
      privEnumClassTake(i)
    }
// CHECK: define {{.*}}swiftcc void @"{{.*}}extEnumCls{{.*}}"
// CHECK:   %[[uEnumClass:.*]] = invoke [[utEnumClass:.*]] @{{.*}}publEnumClassMake{{.*}}
// CHECK:   invoke void @{{.*}}publEnumClassTake{{.*}}({{.*}}, [[utEnumClass]] %[[uEnumClass]])
// CHECK:   %[[iEnumClass:.*]] = invoke [[itEnumClass:.*]] @{{.*}}privEnumClassMake{{.*}}
// CHECK:   invoke void @{{.*}}privEnumClassTake{{.*}}({{.*}}, [[itEnumClass]] %[[iEnumClass]])
}
