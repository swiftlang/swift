// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -enable-objc-interop -emit-ir | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// SR-1055

// CHECK-64: @_DATA__TtC23objc_class_empty_fields14OneEnumWrapper = private constant { {{.*}}* } { i32 {{[0-9]+}}, i32 16, i32 24
// CHECK-32: @_DATA__TtC23objc_class_empty_fields14OneEnumWrapper = private constant { {{.*}}* } { i32 {{[0-9]+}}, i32 8, i32 12

enum OneCaseEnum {
    case X
}

class OneEnumWrapper {
    var myVar: OneCaseEnum
    var whyVar: OneCaseEnum
    var x: Int

    init(v: OneCaseEnum)
    {
        self.myVar = v
        self.whyVar = v
        self.x = 0
    }
}

let e = OneCaseEnum.X
print(e)
let x = OneEnumWrapper(v: e)
print(x)
