// RUN: %target-swift-frontend -c -module-name test -validate-tbd-against-ir %s

// Top-level code (i.e. implicit `main`) should be handled
