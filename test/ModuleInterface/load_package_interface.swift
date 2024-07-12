// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Generate a package swiftinterface. Note -package-name is repeated; the last value should be picked.
// RUN: %target-swift-frontend -emit-module %t/Bar.swift -I %t \
// RUN:   -module-name Bar -package-name foopkg -package-name barpkg \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-interface-path %t/Bar.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Bar.private.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/Bar.package.swiftinterface

/// Client should load a package interface module if enabled with -experimental-package-interface-load
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name barpkg \
// RUN:   -experimental-package-interface-load \
// RUN:   -Rmodule-loading 2> %t/load-pkg-flag.output
// RUN: %FileCheck -check-prefix=CHECK-LOAD-PKG-ENABLED %s < %t/load-pkg-flag.output

/// Client should load a package interface module if enabled with env var `SWIFT_ENABLE_PACKAGE_INTERFACE_LOAD`
// RUN: env SWIFT_ENABLE_PACKAGE_INTERFACE_LOAD=true \
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name barpkg \
// RUN:   -Rmodule-loading 2> %t/load-pkg-env-var.output
// RUN: %FileCheck -check-prefix=CHECK-LOAD-PKG-ENABLED %s < %t/load-pkg-env-var.output

// CHECK-LOAD-PKG-ENABLED: loaded module 'Bar'; source: '{{.*}}Bar.package.swiftinterface', loaded: '{{.*}}Bar-{{.*}}.swiftmodule'

/// Client should not load a package interface without the flag or the env var above;
/// in such case, private swiftinterface is loaded but an error should be thrown in typecheck.
// RUN: not %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name barpkg \
// RUN:   -Rmodule-loading 2> %t/load-pkg-off.output
// RUN: %FileCheck -check-prefix=CHECK-LOAD-PKG-OFF %s < %t/load-pkg-off.output
// CHECK-LOAD-PKG-OFF: error: module 'Bar' is in package 'barpkg' but was built from a non-package interface; modules of the same package can only be loaded if built from source or package interface:{{.*}}Bar.private.swiftinterface

/// Client loads a private interface since the package-name is different from the loaded module's.
// RUN: not %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name foopkg \
// RUN:   -experimental-package-interface-load \
// RUN:   -Rmodule-loading 2> %t/load-diff-pkg.output
// RUN: %FileCheck -check-prefix=CHECK-LOAD-DIFF-PKG %s < %t/load-diff-pkg.output
// CHECK-LOAD-DIFF-PKG: remark: loaded module 'Bar'; source: '{{.*}}Bar.private.swiftinterface', loaded: '{{.*}}Bar-{{.*}}.swiftmodule'
// CHECK-LOAD-DIFF-PKG: error: cannot find 'PkgKlass' in scope; did you mean 'PubKlass'?

// RUN: rm -rf %t/*.swiftmodule
// RUN: rm -rf %t/Bar.package.swiftinterface

/// Client loads a private interface since package interface doesn't exist. It should error since the loaded module is not built from a package interface.
// RUN: not %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name barpkg \
// RUN:   -experimental-package-interface-load \
// RUN:   -Rmodule-loading 2> %t/load-priv.output
// RUN: %FileCheck -check-prefix=CHECK-LOAD-PRIV %s < %t/load-priv.output
// CHECK-LOAD-PRIV: no such module 'Bar'

// RUN: rm -rf %t/*.swiftmodule
// RUN: rm -rf %t/Bar.private.swiftinterface

/// Client loads a public interface since package or private interface doesn't exist.
/// It should error since the loaded module is not built from a package interface.
// RUN: not %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name barpkg \
// RUN:   -experimental-package-interface-load \
// RUN:   -Rmodule-loading 2> %t/load-pub.output

// RUN: %FileCheck -check-prefix=CHECK-LOAD-PUB %s < %t/load-pub.output
// CHECK-LOAD-PUB: no such module 'Bar'

//--- Bar.swift
public class PubKlass {
  public var pubVar = "1"
  public init() {}
}

package class PkgKlass {
  package var pkgVar = "2"
  package init() {}
}

//--- Client.swift
import Bar

func foo() {
  print(PubKlass().pubVar, PkgKlass().pkgVar)
}
