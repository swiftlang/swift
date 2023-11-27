// RUN: %target-swift-frontend %s -emit-module -emit-objc-header-path %t/Issue70016-Swift.h -module-name Issue70016 -cxx-interoperability-mode=default

public struct Struct {}
public func foo(_ x: Struct) {}
