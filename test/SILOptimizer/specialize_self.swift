
// RUN: %target-swift-frontend -module-name specialize_self -Xllvm -sil-full-demangle  -O -sil-inline-threshold 0 -emit-sil -primary-file %s | %FileCheck %s

// CHECK-NOT: generic specialization <Swift.AnyObject, Self> of specialize_self.cast <A, B>(A) -> Swift.Optional<B>

// CHECK-LABEL: specialize_self.cast<A, B>(A) -> Swift.Optional<B>
// CHECK-NEXT: Isolation: unspecified
// CHECK-NEXT: sil hidden @$s15specialize_self4cast{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T, R> (@in_guaranteed T) -> @out Optional<R>
func cast<T,R>(_ x: T) -> R? {
  return x as? R
}

// CHECK-LABEL: static specialize_self.Base.returnIfSelf(Swift.AnyObject) -> Swift.Optional<Self>
// CHECK-NEXT: Isolation: unspecified
// CHECK-NEXT: sil hidden @$s15specialize_self4BaseC12returnIfSelf{{[_0-9a-zA-Z]*}}FZ : $@convention(method) (@guaranteed AnyObject, @thick Base.Type) -> @owned Optional<Base>
// CHECK: [[CAST:%[0-9]+]] = function_ref @$s15specialize_self4cast{{[_0-9a-zA-Z]*}}F
// CHECK: apply [[CAST]]<AnyObject, @dynamic_self Base>
class Base {
  class func returnIfSelf(_ x: AnyObject) -> Self? {
    return cast(x)
  }
}
