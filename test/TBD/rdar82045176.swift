// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(first)) %s -emit-module -module-name thing -emit-tbd -enable-testing -Xfrontend -disable-availability-checking -Xfrontend -validate-tbd-against-ir=all

class Test {
  init() async { }
}
