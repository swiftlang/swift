// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -import-objc-header %S/Inputs/serialized-objc-header.h -emit-ir -g -o - | %FileCheck %s

// REQUIRES: objc_interop

protocol Named {
    var name : String { get }
}

// initializer.Person.init (initializer.Person.Type)() -> initializer.Person
// CHECK: define hidden {{.*}}%C11initializer6Person* @_T011initializer6PersonCACycfc(%C11initializer6Person*{{.*}}) {{.*}} {

// initializer.Person.__allocating_init (initializer.Person.Type)() -> initializer.Person
// CHECK: define hidden {{.*}}%C11initializer6Person* @_T011initializer6PersonCACycfC(%swift.type*{{.*}}) {{.*}} {
// CHECK:  call {{.*}}%C11initializer6Person* @_T011initializer6PersonCACycfc(%C11initializer6Person* {{.*}}%3), !dbg ![[ALLOCATING_INIT:.*]]

// CHECK-DAG: ![[ALLOCATING_INIT]]  = !DILocation(line: 0, scope
class Person : Named {
    var name : String { get { return "No Name" } }
    var age = 0
}

var person = Person()
