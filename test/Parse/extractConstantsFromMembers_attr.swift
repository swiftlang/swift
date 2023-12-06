// RUN: %target-typecheck-verify-swift -parse -enable-experimental-feature ExtractConstantsFromMembers

// REQUIRES: asserts

@extractConstantsFromMembers
protocol MyProto {}
