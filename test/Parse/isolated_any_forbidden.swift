// RUN: %target-typecheck-verify-swift 

typealias FnType = @isolated(any) () -> ()
// UNSUPPORTED: OS=windows-msvc
