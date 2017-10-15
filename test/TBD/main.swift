// RUN: %target-swift-frontend -emit-ir -o- -module-name test -validate-tbd-against-ir=all %s

// Top-level code (i.e. implicit `main`) should be handled
