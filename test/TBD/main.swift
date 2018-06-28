// RUN: %target-swift-frontend -emit-ir -o- -module-name test -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -emit-ir -o- -module-name test -validate-tbd-against-ir=all %s -O

// Top-level code (i.e. implicit `main`) should be handled
