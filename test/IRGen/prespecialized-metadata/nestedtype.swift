// RUN: %swift -prespecialize-generic-metadata -target %module-target-future  -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

struct Container<T> {
  struct NonFixed {

    init( _ t: T) {
        w = t
    }

    var w: T? = nil
    var x = 1
    var y = 1
    var z = 1
  }
}


func doit2() {
  consume(Container.NonFixed(5))
}

doit2()

// Make sure we use the generic witness table functions rather than specialized
// ones. We do this for code size.

// CHECK: @"$s10nestedtype9ContainerV8NonFixedVySi_GWV" =
// CHECK-SAME: s10nestedtype9ContainerV8NonFixedVwtk
