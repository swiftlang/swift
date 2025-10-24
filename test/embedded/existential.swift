// RUN: %target-swift-frontend -enable-experimental-feature EmbeddedExistentials -enable-experimental-feature Embedded -parse-as-library -wmo -emit-sil %s | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature EmbeddedExistentials -enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s --check-prefix=OUTPUT

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedExistentials

class CP {
}

class C : CP {
    func foo() { }
}

class GC<T> {
    var x: T? = nil
    func foo() {}
    deinit {
        print("deinit called")
    }
}

struct StructWithClass {
    var c = GC<Int>()
}

struct GenericStructWithClass<T> {
    var c = GC<T>()
    var d = GC<T>()
}

enum EnumWithClass {
    case a
    case c(GC<Int>)
}

enum GenericEnumWithClass<T> {
    case a
    case c(GC<T>)
}

// CHECK: sil @$e11existential4testyyF
// CHECK: init_existential_addr
// CHECK: } // end sil function '$e11existential4testyyF'

// There are 8 class instances that are destroyed.

// OUTPUT:  deinit called
// OUTPUT:  deinit called
// OUTPUT:  deinit called
// OUTPUT:  deinit called

// OUTPUT:  deinit called
// OUTPUT:  deinit called
// OUTPUT:  deinit called
// OUTPUT:  deinit called

// OUTPUT-NOT:  deinit called

func test() {
    let _: any Any = GC<Int>()
    let _: any Any = 3
    let _: any Any = StructWithClass()
    let _: any Any = GenericStructWithClass<Int>()
    let _: any Any = EnumWithClass.c(GC<Int>())
    let _: any Any = GenericEnumWithClass.c(GC<Int>())
    let _: any Any = (3, 4)
    let _: any Any = (StructWithClass(), StructWithClass())
}

@main
struct Main {
    static func main() {
        test()
    }
}
