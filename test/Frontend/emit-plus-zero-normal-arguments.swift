// RUN: %target-swift-frontend -module-name Swift -emit-silgen %s

class Klass {}

// CHECK-LABEL: sil hidden @$s4main3fooyyAA5KlassCF : $@convention(thin) (@guaranteed Klass) -> () {
func foo(_ k: Klass) {}

