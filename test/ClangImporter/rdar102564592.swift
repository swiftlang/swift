// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend -typecheck -disable-objc-attr-requires-foundation-module -import-objc-header %t/src/ObjC.h -O %t/src/main.swift

// REQUIRES: objc_interop

//--- ObjC.h

@interface MyUnit
@end

__attribute__((swift_name("Metrics.SomeMetric")))
@interface SomeMetric <T: MyUnit *>
@end

@interface Metrics
@property (readonly, strong) SomeMetric<MyUnit *> *metric;
@end

//--- main.swift
func test(metrics: Metrics) -> Metrics.SomeMetric<MyUnit> {
  metrics.metric
}

