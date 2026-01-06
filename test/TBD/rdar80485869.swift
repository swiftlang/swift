// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-module-path %t/module -emit-tbd -enable-testing -target %target-swift-5.1-abi-triple -tbd-install_name test 

public actor Tom {
    init() async {
    }
}
