// RUN: %target-typecheck-verify-swift

@_nonatomicReferenceCounting class C {}
@_nonatomicReferenceCounting final class D: C {}
@_nonatomicReferenceCounting class G<T> { var t: T? }

@_nonatomicReferenceCounting struct S {} // expected-error {{@_nonatomicReferenceCounting may only be used on 'class' declarations}}
@_nonatomicReferenceCounting enum E {} // expected-error {{@_nonatomicReferenceCounting may only be used on 'class' declarations}}
@_nonatomicReferenceCounting func f() {} // expected-error {{@_nonatomicReferenceCounting may only be used on 'class' declarations}}
