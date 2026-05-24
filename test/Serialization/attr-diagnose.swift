// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -emit-module-path %t/a.swiftmodule -module-name a %s
// RUN: %llvm-bcanalyzer -dump %t/a.swiftmodule | %FileCheck --check-prefix BC-CHECK --implicit-check-not UnknownCode %s
// RUN: %target-swift-ide-test -print-module -module-to-print a -source-filename x -I %t | %FileCheck --check-prefix MODULE-CHECK %s

// @diagnose is a source-local diagnostic control and should not be serialized into the binary module (matching how it is excluded from textual interfaces).

// BC-CHECK-NOT: Diagnose_DECL_ATTR
// MODULE-CHECK-NOT: @diagnose
// MODULE-CHECK: func callsDeprecated()

@available(*, deprecated)
public func deprecated() {}

@inlinable
@diagnose(DeprecatedDeclaration, as: ignored)
public func callsDeprecated() {
  deprecated()
}
