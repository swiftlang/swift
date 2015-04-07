// RUN: %target-swift-frontend %s -emit-ir

// Test case submitted to project by https://github.com/tmu (Teemu Kurppa)
// rdar://18174611

"".join(["ab","cd"])
