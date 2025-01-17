// RUN: %empty-directory(%t)
// RUN: %llvm-nm -g --defined-only -f just-symbols %stdlib_dir/x86_64/libswift_Builtin_float.dylib > %t/symbols
// RUN: %abi-symbol-checker %s %t/symbols
// RUN: diff -u %S/../../Inputs/macOS/x86_64/builtin_float/baseline %t/symbols

// REQUIRES: swift_stdlib_no_asserts
// REQUIRES: STDLIB_VARIANT=macosx-x86_64

// *** DO NOT DISABLE OR XFAIL THIS TEST. *** (See comment below.)

// Welcome, Build Wrangler!
//
// This file lists APIs that have recently changed in a way that potentially
// indicates an ABI- or source-breaking problem.
//
// A failure in this test indicates that there is a potential breaking change in
// the Standard Library. If you observe a failure outside of a PR test, please
// reach out to the Standard Library team directly to make sure this gets
// resolved quickly! If your own PR fails in this test, you probably have an
// ABI- or source-breaking change in your commits. Please go and fix it.
//
// Please DO NOT DISABLE THIS TEST. In addition to ignoring the current set of
// ABI breaks, XFAILing this test also silences any future ABI breaks that may
// land on this branch, which simply generates extra work for the next person
// that picks up the mess.
//
// Instead of disabling this test, you'll need to extend the list of expected
// changes at the bottom. (You'll also need to do this if your own PR triggers
// false positives, or if you have special permission to break things.) You can
// find a diff of what needs to be added in the output of the failed test run.
// The order of lines doesn't matter, and you can also include comments to refer
// to any bugs you filed.
//
// Thank you for your help ensuring the stdlib remains compatible with its past!
//                                            -- Your friendly stdlib engineers

// _Builtin_float Symbols
