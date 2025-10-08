// REQUIRES: objc_interop

// FIXME: the test output we're comparing to is specific to macOS.
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays

// RUN: %sourcekitd-test -req=doc-info -module Foo -- -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         -target %target-triple %clang-importer-sdk-nosource -I %t | %sed_clean > %t.response
// RUN: sed -e 's/ file=[^*.]*.swift[^>]*>/>/g' %t.response > %t.response.cleaned
// RUN: %diff -u %s.response %t.response.cleaned

// RUN: %sourcekitd-test -req=doc-info -module Foo.FooSub -- -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         -target %target-triple %clang-importer-sdk-nosource -I %t | %sed_clean > %t.response
// RUN: sed -e 's/ file=[^*.]*.swift[^>]*>/>/g' %t.response > %t.response.cleaned
// RUN: %diff -u %s.sub.response %t.response.cleaned
