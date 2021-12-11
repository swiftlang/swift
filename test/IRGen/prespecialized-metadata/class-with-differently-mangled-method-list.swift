// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -enable-library-evolution -prespecialize-generic-metadata -target %module-target-future -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment --check-prefix=CHECK --check-prefix=CHECK-%target-vendor

// REQUIRES: VENDOR=apple || OS=linux-gnu
// REQUIRES: objc_interop
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

import Foundation

struct S{}
class Clazz<T> {
  @objc func foo() {}
}


@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in }
}

func doit() {
  //      CHECK: @"_INSTANCE_METHODS_$s4main5ClazzCyAA1SVGMf" = internal constant
  consume(Clazz<S>.self)
}

doit()
