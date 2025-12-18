// RUN: not %target-swift-frontend -parse %s

// https://github.com/apple/swift/issues/60702

@
#if true
    print("x")
#endif
