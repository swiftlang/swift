// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(first)) %s -emit-module -module-name thing -emit-tbd -enable-testing -target %target-swift-5.1-abi-triple -Xfrontend -validate-tbd-against-ir=all -Xfrontend -tbd-install_name -Xfrontend thing

class Test {
  init() async { }
}
