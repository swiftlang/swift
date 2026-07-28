// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -parse-as-library %s
// RUN: %target-swift-emit-silgen -parse-as-library %s | %FileCheck %s

@main enum Horse {
  static func main() {}
}

// CHECK-LABEL: sil hidden [ossa] @$s14attr_main_enum5HorseO5$mainyyFZ : $@convention(method) (@thin Horse.Type) -> () {

// CHECK-LABEL: sil [ossa] @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {
// CHECK: function_ref @$s14attr_main_enum5HorseO5$mainyyFZ : $@convention(method) (@thin Horse.Type) -> ()
// CHECK: }
