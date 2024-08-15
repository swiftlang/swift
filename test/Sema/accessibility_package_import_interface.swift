// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// TEST Build Dep with package symbols
// RUN: %target-swift-frontend -emit-module %t/Dep.swift \
// RUN:   -module-name Dep -swift-version 5 -I %t \
// RUN:   -package-name myPkg \
// RUN:   -enable-library-evolution \
// RUN:   -disable-print-package-name-for-non-package-interface \
// RUN:   -emit-module-path %t/Dep.swiftmodule \
// RUN:   -emit-module-interface-path %t/Dep.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Dep.private.swiftinterface

// TEST Dep.swiftmodule should contain package name and package symbols
// RUN: llvm-bcanalyzer --dump %t/Dep.swiftmodule | %FileCheck %s --check-prefix=CHECK-DEP-BC
// CHECK-DEP-BC: <MODULE_PACKAGE_NAME {{.*}}> blob data = 'myPkg'

// TEST Lib should load Dep.swiftmodule and access package decls if in the same package and error if not
// RUN: %target-swift-frontend -typecheck %t/Lib.swift -package-name myPkg -I %t

// RUN: not %target-swift-frontend -typecheck %t/Lib.swift -package-name otherPkg -I %t -Rmodule-loading 2> %t/result-binary-other-pkg.output
// RUN: %FileCheck %s --check-prefix=CHECK-DIFF-PKG < %t/result-binary-other-pkg.output

// RUN: not %target-swift-frontend -typecheck %t/Lib.swift -I %t -Rmodule-loading 2> %t/result-binary-no-pkg.output
// RUN: %FileCheck %s --check-prefix=CHECK-DIFF-PKG < %t/result-binary-no-pkg.output

// CHECK-DIFF-PKG: remark: loaded module 'Dep'
// CHECK-DIFF-PKG: error: cannot find 'packageFuncInlinable' in scope
// CHECK-DIFF-PKG: error: cannot find 'packageFunc' in scope
// CHECK-DIFF-PKG: error: cannot find 'PackageKlassUFI' in scope

// TEST Remove Dep binary and build it from interface
// RUN: rm %t/Dep.swiftmodule
// RUN: %target-swift-frontend -compile-module-from-interface %t/Dep.private.swiftinterface \
// RUN:   -module-name Dep -I %t \
// RUN:   -o %t/Dep.swiftmodule

// TEST Dep binary built from interface should not contain package name or package symbols.
// RUN: llvm-bcanalyzer --dump %t/Dep.swiftmodule | %FileCheck %s --check-prefix=CHECK-DEP-INTER-BC
// CHECK-DEP-INTER-BC-NOT: <MODULE_PACKAGE_NAME {{.*}}> blob data = 'myPkg'

// TEST Lib should error on loading Dep built from interface and accessing package symbols (unless usableFromInline or inlinable)
// RUN: %target-swift-frontend -typecheck %t/Lib.swift -package-name myPkg -I %t -verify

// TEST Remove and rebuild Dep from source
// RUN: rm %t/Dep.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Dep.swift \
// RUN:   -module-name Dep -swift-version 5 -I %t \
// RUN:   -package-name myPkg \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/Dep.swiftmodule

// TEST Build LibPass with package name
// RUN: %target-swift-frontend -emit-module %t/LibPass.swift \
// RUN:   -module-name LibPass -swift-version 5 -I %t \
// RUN:   -package-name myPkg \
// RUN:   -enable-library-evolution \
// RUN:   -disable-print-package-name-for-non-package-interface \
// RUN:   -emit-module-path %t/LibPass.swiftmodule \
// RUN:   -emit-module-interface-path %t/LibPass.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/LibPass.private.swiftinterface

// TEST Loading LibPass and accessing lib func should pass
// RUN: %target-swift-frontend -typecheck %t/Client.swift -package-name myPkg -I %t -verify

// TEST Building LibPass from interface with Dep (built from interface) should succeed with or without package name
// Without package name
// RUN: rm %t/Dep.swiftmodule
// RUN: rm %t/LibPass.swiftmodule
// RUN: %target-swift-frontend -compile-module-from-interface %t/LibPass.private.swiftinterface \
// RUN:   -module-name LibPass -I %t \
// RUN:   -o %t/LibPass.swiftmodule

// With package name
// RUN: rm %t/LibPass.swiftmodule
// RUN: %target-swift-frontend -compile-module-from-interface %t/LibPass.private.swiftinterface \
// RUN:   -module-name LibPass -I %t \
// RUN:   -package-name myPkg \
// RUN:   -o %t/LibPass.swiftmodule


//--- Dep.swift
@usableFromInline
package class PackageKlassUFI {
  @usableFromInline package init() {}
  package var packageVar: String = "pkg"

  @usableFromInline package var packageVarUFI: String = "pkgUFI"
  // expected-note@-1 * {{'packageVarUFI' declared here}}
  // expected-error@-1 * {{'packageVarUFI' declared here}}
}

package func packageFunc() {
  print("package func")
}

@inlinable
package func packageFuncInlinable() {
  print("inlinable package func")
}

public func publicFunc() {
  print("public func")
}

@inlinable
public func publicFuncInlinable() {
  print("inlinable public func")
}

//--- Lib.swift
import Dep

public func libFunc() {
  publicFuncInlinable()
  publicFunc()
  packageFuncInlinable()
  packageFunc() // expected-error {{cannot find 'packageFunc' in scope}}
  let x = PackageKlassUFI()
  let y = x.packageVarUFI
  print(x, y)
}


//--- LibPass.swift
import Dep

public func libFunc() {
  publicFuncInlinable()
  publicFunc()
  packageFuncInlinable()
  let x = PackageKlassUFI()
  let y = x.packageVarUFI
  print(x, y)
}


//--- Client.swift
import LibPass

public func clientFunc() {
  libFunc()
}
