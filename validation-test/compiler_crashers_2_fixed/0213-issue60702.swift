// RUN: not %target-swift-frontend -parse %s
@
#if true
    print("x")
#endif
