// RUN: not %target-swift-frontend -emit-sil %s -emit-fixits-path %t.remap -diagnostics-editor-mode
// RUN: c-arcmt-test %t.remap | arcmt-test -verify-transformed-files %s.result

func ff_fixit_addreturn() -> String {
    print("entering ff_fixit_addreturn()")
    "foo"
}

let cl_fixit_addreturn: () -> String = {
    print("entering cl_fixit_addreturn()")
    "foo"
}


