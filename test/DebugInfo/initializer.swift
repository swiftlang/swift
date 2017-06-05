// RUN: %target-swift-frontend %s -import-objc-header %S/Inputs/serialized-objc-header.h -emit-ir -g -o - | %FileCheck %s

// REQUIRES: objc_interop

protocol Named {
    var name : String { get }
}

// initializer.Person.__allocating_init (initializer.Person.Type)() -> initializer.Person
// CHECK: define hidden {{.*}}%T11initializer6PersonC* @_T011initializer6PersonCACycfC(%swift.type*{{.*}}) {{.*}} {
// CHECK:  call {{.*}}%T11initializer6PersonC* @_T011initializer6PersonCACycfc(%T11initializer6PersonC* {{.*}}%3), !dbg ![[ALLOCATING_INIT:.*]]

// initializer.Person.init (initializer.Person.Type)() -> initializer.Person
// CHECK: define hidden {{.*}}%T11initializer6PersonC* @_T011initializer6PersonCACycfc(%T11initializer6PersonC*{{.*}}) {{.*}} {

// CHECK-DAG: ![[ALLOCATING_INIT]]  = !DILocation(line: 0, scope
class Person : Named {
    var name : String { get { return "No Name" } }
    var age = 0
}

var person = Person()
