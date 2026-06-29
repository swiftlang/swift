// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -parse-as-library %s
// RUN: %target-swift-emit-silgen -parse-as-library %s | %FileCheck %s

@main class Horse {
  class func main() {}
}

// CHECK-LABEL: sil hidden [ossa] @$s17attr_main_class_25HorseC5$mainyyFZ : $@convention(method) (@thick Horse.Type) -> () {

// CHECK-LABEL: sil [ossa] @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {
// CHECK: function_ref @$s17attr_main_class_25HorseC5$mainyyFZ : $@convention(method) (@thick Horse.Type) -> ()
// CHECK: }
