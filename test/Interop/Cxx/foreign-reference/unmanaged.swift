// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown -verify-ignore-unrelated -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking %s

// Foreign reference types do not satisfy `AnyObject`, so they cannot be
// used with `Unmanaged<T : AnyObject>.

import POD

extension Empty {
   public static func == (lhs: Empty, rhs: Empty) -> Bool {
        Unmanaged.passUnretained(lhs).toOpaque() == Unmanaged.passUnretained(rhs).toOpaque()
        // expected-error@-1 6 {{generic struct 'Unmanaged' requires that 'Empty' be a class type}}
   }
}
