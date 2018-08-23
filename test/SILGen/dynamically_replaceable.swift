// RUN: %target-swift-emit-silgen -enable-sil-ownership -swift-version 5 %s | %FileCheck %s


// CHECK-LABEL: sil hidden [dynamically_replacable] @$s23dynamically_replaceable08dynamic_B0yyF : $@convention(thin) () -> () {
dynamic func dynamic_replaceable() {
}

// CHECK-LABEL: sil hidden [dynamically_replacable] @$s23dynamically_replaceable6StruktV08dynamic_B0yyF : $@convention(method) (Strukt) -> () {
struct Strukt {
  dynamic func dynamic_replaceable() {
  }
}

// CHECK: sil hidden [dynamically_replacable] @$s23dynamically_replaceable5KlassC08dynamic_B0yyF : $@convention(method) (@guaranteed Klass) -> () {
class Klass {
  dynamic func dynamic_replaceable() {
  }
}

// CHECK-LABEL: sil hidden [dynamically_replacable] @$s23dynamically_replaceable6globalSivg : $@convention(thin) () -> Int {
dynamic var global : Int {
  return 1
}
