// RUN: %target-swift-frontend  -O -sil-inline-threshold 0 -emit-sil -primary-file %s | FileCheck %s

// CHECK-NOT: generic specialization <Swift.AnyObject, Self> of specialize_self.cast <A, B>(A) -> Swift.Optional<B>

// CHECK-LABEL: specialize_self.cast <A, B> (A) -> Swift.Optional<B>
// CHECK-NEXT: sil hidden @_TF15specialize_self4cast{{.*}} : $@convention(thin) <T, R> (@out Optional<R>, @in T) -> ()
func cast<T,R>(x: T) -> R? {
  return x as? R
}

// CHECK-LABEL: static specialize_self.Base.returnIfSelf (Swift.AnyObject) -> Swift.Optional<Self>
// CHECK-NEXT: sil hidden @_TZFC15specialize_self4Base12returnIfSelf{{.*}} : $@convention(thin) (@owned AnyObject, @thick Base.Type) -> @owned Optional<Base>
// CHECK: [[CAST:%[0-9]+]] = function_ref @_TF15specialize_self4cast
// CHECK: apply [[CAST]]<AnyObject, Self>
class Base {
  class func returnIfSelf(x: AnyObject) -> Self? {
    return cast(x)
  }
}
