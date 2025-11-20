// RUN: %target-swift-frontend %s  -O -Xllvm -sil-print-types -Xllvm -sil-print-after=loadable-address -c -o %t/t.o 2>&1 | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// wasm currently disables aggressive reg2mem
// UNSUPPORTED: CPU=wasm32

public struct LargeThing {
    var  s0 : String = ""
    var  s1 : String = ""
    var  s2 : String = ""
    var  s3 : String = ""
    var  s4 : String = ""
    var  s5 : String = ""
    var  s6 : String = ""
    var  s7 : String = ""
    var  s8 : String = ""

    public init() {}

    mutating func setThirdString(_ to: String) {
        s2 = to
    }

    var thirdString : String {
        return s2
    }
}


public struct Container {
    public var field : LargeThing
    public var field2 : LargeThing

    public init(_ l: LargeThing, _ l2: LargeThing) {
        field = l
        field2 = l2
    }

}

public struct Container2 {
    public var field : Container
    public var field2: Container
    public var field3: LargeThing?

    public init(_ b: Bool, _ l: LargeThing, _ l2: LargeThing) {
        if b {
            let t = Container(l, l2)
            let t2 = Container(l2, l)
            field2 = t2
            field = t
            field3 = t.field
        } else {
            let t = Container(l, l2)
            let t2 = Container(l2, l)
            field = t2
            field2 = t
            field3 = t2.field2
        }
    }

    public func testLargeThing() {
        if let x = field3 {
            print("hello \(x)")
        }
    }
}

// CHECK: sil @$s1t10LargeThingVACycfC : $@convention(method) (@thin LargeThing.Type) -> @out LargeThing {
// CHECK: bb0(%0 : $*LargeThing, %1 : $@thin LargeThing.Type):
// CHECK:  [[T0:%.*]] = alloc_stack $LargeThing
// CHECK:  [[T1:%.*]] = alloc_stack [var_decl] $LargeThing
// CHECK:  [[T2:%.*]] = struct_element_addr [[T1]] : $*LargeThing, #LargeThing.s0
// CHECK:  store {{.*}} to [[T2]] : $*String
// CHECK:  copy_addr [take] [[T1]] to [init] [[T0]] : $*LargeThing
// CHECK:  copy_addr [take] [[T0]] to [init] %0 : $*LargeThing
// CHECK: } // end sil function '$s1t10LargeThingVACycfC'


// CHECK: sil [transparent] @$s1t9ContainerV5fieldAA10LargeThingVvg : $@convention(method) (@in_guaranteed Container) -> @out LargeThing {
// CHECK: bb0(%0 : $*LargeThing, %1 : $*Container):
// CHECK:   [[T0:%.*]] = struct_element_addr %1 : $*Container, #Container.field
// CHECK:   copy_addr [[T0]] to [init] %0 : $*LargeThing
// CHECK: } // end sil function '$s1t9ContainerV5fieldAA10LargeThingVvg'

// CHECK: sil [transparent] @$s1t10Container2V5fieldAA9ContainerVvs : $@convention(method) (@in Container, @inout Container2) -> () {
// CHECK: bb0(%0 : $*Container, %1 : $*Container2):
// CHECK:   [[T0:%.*]] = struct_element_addr %1 : $*Container2, #Container2.field
// CHECK:   copy_addr [take] %0 to [[T0]] : $*Container
// CHECK: } // end sil function '$s1t10Container2V5fieldAA9ContainerVvs'

// CHECK: sil @$s1t10Container2VyACSb_AA10LargeThingVAEtcfC :
// CHECK:   [[T0:%.*]] = init_enum_data_addr [[T2:%.*]] : $*Optional<LargeThing>, #Optional.some!enumelt
// CHECK:   copy_addr [take] {{.*}} to [init] [[T0]] : $*LargeThing
// CHECK:   inject_enum_addr [[T2]] : $*Optional<LargeThing>, #Optional.some!enumelt
// CHECK: } // end sil function '$s1t10Container2VyACSb_AA10LargeThingVAEtcfC
