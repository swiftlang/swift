// RUN: %target-swift-emit-silgen -enable-experimental-move-only -parse-stdlib -disable-availability-checking %s | %FileCheck %s
// RUN: %target-swift-emit-sil -enable-experimental-move-only -parse-stdlib -disable-availability-checking %s | %FileCheck -check-prefix=CHECK-SIL %s
// RUN: %target-swift-emit-sil -O -enable-experimental-move-only -Xllvm -sil-disable-pass=FunctionSignatureOpts -parse-stdlib -disable-availability-checking %s | %FileCheck -check-prefix=CHECK-SIL %s

public class Klass {
    func doSomething() {}
}

// CHECK: bb0(%0 : @noImplicitCopy @guaranteed $Klass):
// CHECK-SIL: bb0(%0 : @noImplicitCopy $Klass):

public func arguments(@_noImplicitCopy _ x: Klass) {
    x.doSomething()
}

// CHECK: bb0(%0 : @noImplicitCopy @owned $Klass):
// CHECK-SIL: bb0(%0 : @noImplicitCopy $Klass):
public func argumentsOwned(@_noImplicitCopy _ x: __owned Klass) {
    x.doSomething()
}

extension Klass {
// CHECK-LABEL: sil hidden [ossa] @noimplicitcopy_method_attr : $@convention(method) (@owned Klass) -> () {
// CHECK:       {{bb[0-9]+}}({{%[^,]+}} : @noImplicitCopy @owned $Klass):
// CHECK-LABEL: } // end sil function 'noimplicitcopy_method_attr'
  @_silgen_name("noimplicitcopy_method_attr")
  @_noImplicitCopy
  __consuming
  func noimplicitcopy_method_attr() {}
}
