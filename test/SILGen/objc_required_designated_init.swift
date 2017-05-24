// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/objc_required_designated_init_2.swift -module-name Booms -o %t/Booms.swiftmodule -import-objc-header %S/Inputs/objc_required_designated_init.h
// RUN: %target-swift-frontend -I %t -emit-silgen -verify %s -import-objc-header %S/Inputs/objc_required_designated_init.h

// REQUIRES: objc_interop

import Booms

class Baboom: Boom {
  required init() {
    super.init()
  }
}

class BigBadaBoom<V>: Badaboom<V> {
  required init() {
    super.init()
  }
}
