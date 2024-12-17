// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=default
import InheritedLookup

extension IIBase1 {
    func ext() {
        // NOTE: we deliberately look up a missing member above because doing so
        // forces multiple ClangRecordMemberLookup requests, which should be
        // idempotent (though this hasn't always been the case, because bugs).
        missing() // expected-error {{cannot find 'missing' in scope}}

        // For instance, a non-idempotent ClangRecordMemberLookup would cause
        // the following to appear ambiguous:
        methodBase()
        methodIBase()
        methodIIBase()
    }
}

func f(v: IIBase1) {
    v.missing() // expected-error {{'IIBase1' has no member 'missing'}}
    v.methodBase()
    v.methodIBase()
    v.methodIIBase()
}
