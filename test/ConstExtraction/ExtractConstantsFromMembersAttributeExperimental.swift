// RUN: %target-typecheck-verify-swift %s

// expected-error@+1{{'@extractConstantsFromMembers' requires '-enable-experimental-feature ExtractConstantsFromMembers'}}
@extractConstantsFromMembers protocol MyProto {}
