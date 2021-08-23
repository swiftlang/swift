// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-module-path %t/module -emit-tbd -enable-testing -disable-availability-checking

public actor Tom {
    init() async {
    }
}
