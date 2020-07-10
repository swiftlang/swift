// RUN: %target-swift-emit-silgen -parse-as-library %s | %FileCheck %s

@main struct Horse {
  static func main() {}
}

// CHECK-LABEL: sil hidden [ossa] @$s16attr_main_struct5HorseV5$mainyyFZ : $@convention(method) (@thin Horse.Type) -> () {

// CHECK-LABEL: sil [ossa] @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {
// CHECK: function_ref @$s16attr_main_struct5HorseV5$mainyyFZ : $@convention(method) (@thin Horse.Type) -> ()
// CHECK: }
