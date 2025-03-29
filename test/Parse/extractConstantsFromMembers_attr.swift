// RUN: %target-typecheck-verify-swift -parse -enable-experimental-feature ExtractConstantsFromMembers

// REQUIRES: swift_feature_ExtractConstantsFromMembers

@extractConstantsFromMembers
protocol MyProto {}
