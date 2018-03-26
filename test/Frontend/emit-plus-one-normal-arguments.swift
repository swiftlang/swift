// RUN: %target-swift-frontend -disable-guaranteed-normal-arguments -module-name Swift -emit-silgen %s

class Klass {}

// CHECK-LABEL: sil hidden @$S4main3fooyyAA5KlassCF : $@convention(thin) (@owned Klass) -> () {
func foo(_ k: Klass) {}
