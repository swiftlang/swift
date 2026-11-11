// RUN: %target-swift-frontend -emit-ir -module-name main -sanitize=address -sanitize-coverage=func \
// RUN:   %s | %FileCheck %s --check-prefix=COV
// RUN: %target-swift-frontend -emit-ir -module-name main \
// RUN:   %s | %FileCheck %s --check-prefix=NOCOV


// COV: define {{.*}}@"$s4main7withCovSiyF"() [[COV:#[0-9]+]]
public func withCov() -> Int { 0 }

// COV: define {{.*}}@"$s4main5noCovSiyF"() [[NO_COV:#[0-9]+]]
@instrumentation(disable: coverage)
public func noCov() -> Int { 1 }

// COV-NOT: attributes [[COV]] = {{{.*}}nosanitize_coverage
// COV: attributes [[NO_COV]] = {{{.*}}nosanitize_coverage

// Without coverage enabled, the attribute is not emitted at all.
// NOCOV-NOT: nosanitize_coverage
