// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Bar.swift -I %t \
// RUN:   -module-name Bar -package-name barpkg \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -module-cache-path %t/ModuleCache \
// RUN:   -emit-module -emit-module-path %t/Bar.swiftmodule \
// RUN:   -O -wmo -allow-non-resilient-access -package-cmo \
// RUN:   -disable-print-package-name-for-non-package-interface \
// RUN:   -emit-module-interface-path %t/Bar.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Bar.private.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/Bar.package.swiftinterface

// RUN: %target-swift-frontend -typecheck %t/InPkgClient.swift -I %t \
// RUN:   -package-name barpkg \
// RUN:   -Rmodule-loading 2> %t/load-pcmo-module.txt
// RUN: %FileCheck -check-prefix=LOAD-BINARY %s < %t/load-pcmo-module.txt

// RUN: %target-swift-frontend -typecheck %t/ExtClient.swift -I %t \
// RUN:   -Rmodule-loading 2> %t/load-interface.txt
// RUN: %FileCheck -check-prefix=LOAD-INTERFACE %s < %t/load-interface.txt

// LOAD-BINARY: remark: loaded module 'Bar'; source: '{{.*}}Bar.private.swiftinterface', loaded: '{{.*}}Bar.swiftmodule'

// LOAD-INTERFACE: remark: loaded module 'Bar'; source: '{{.*}}Bar.private.swiftinterface', loaded: '{{.*}}Bar-{{.*}}.swiftmodule'

//--- Bar.swift
public class PubKlass {
  public var pubVar = "1"
  public init() {}
}

package class PkgKlass {
  package var pkgVar = "2"
  package init() {}
}

//--- InPkgClient.swift
import Bar

func foo() {
  print(PubKlass().pubVar, PkgKlass().pkgVar)
}

//--- ExtClient.swift
import Bar

func foo() {
  print(PubKlass().pubVar)
}
