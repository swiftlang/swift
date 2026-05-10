/// Ensure we only bump down the access-level of imported decls, not up.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build libraries
// RUN: %target-swift-frontend -emit-module %t/ImportedPrivate.swift \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -package-name pkg \
// RUN:   -emit-module-path %t/ImportedPrivate.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/ImportedInternal.swift \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -package-name pkg \
// RUN:   -emit-module-path %t/ImportedInternal.swiftmodule -I %t
// RUN: %target-swift-frontend -emit-module %t/ImportedPackage.swift \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -package-name pkg \
// RUN:   -emit-module-path %t/ImportedPackage.swiftmodule -I %t
// RUN: %target-swift-frontend -emit-module %t/ImportedPublic.swift \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -package-name pkg \
// RUN:   -emit-module-path %t/ImportedPublic.swiftmodule -I %t

/// Build clients
// RUN: %target-swift-frontend -typecheck %t/InPackageClient.swift -I %t \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -package-name pkg -verify
// RUN: %target-swift-frontend -typecheck %t/OutOfPackageClient.swift -I %t \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -verify

//--- ImportedPrivate.swift

private func privateFunc() {}
internal func internalFunc() {}
package func packageFunc() {}
public func publicFunc() {}

//--- ImportedInternal.swift

private func privateFunc() {}
internal func internalFunc() {}
package func packageFunc() {}
public func publicFunc() {}

//--- ImportedPackage.swift

private func privateFunc() {}
internal func internalFunc() {}
package func packageFunc() {}
public func publicFunc() {}

//--- ImportedPublic.swift

private func privateFunc() {}
internal func internalFunc() {}
package func packageFunc() {}
public func publicFunc() {}

//--- InPackageClient.swift

private import ImportedPrivate
internal import ImportedInternal
private import ImportedPackage
public import ImportedPublic // expected-warning{{public import of 'ImportedPublic' was not used in public declarations or inlinable code}}

ImportedPrivate.privateFunc() // expected-error {{module 'ImportedPrivate' has no member named 'privateFunc'}}
ImportedPrivate.internalFunc() // expected-error {{module 'ImportedPrivate' has no member named 'internalFunc'}}
ImportedPrivate.packageFunc()
ImportedPrivate.publicFunc()

ImportedInternal.privateFunc() // expected-error {{module 'ImportedInternal' has no member named 'privateFunc'}}
ImportedInternal.internalFunc() // expected-error {{module 'ImportedInternal' has no member named 'internalFunc'}}
ImportedInternal.packageFunc()
ImportedInternal.publicFunc()

ImportedPackage.privateFunc() // expected-error {{module 'ImportedPackage' has no member named 'privateFunc'}}
ImportedPackage.internalFunc() // expected-error {{module 'ImportedPackage' has no member named 'internalFunc'}}
ImportedPackage.packageFunc()
ImportedPackage.publicFunc()

ImportedPublic.privateFunc() // expected-error {{module 'ImportedPublic' has no member named 'privateFunc'}}
ImportedPublic.internalFunc() // expected-error {{module 'ImportedPublic' has no member named 'internalFunc'}}
ImportedPublic.packageFunc()
ImportedPublic.publicFunc()

func funcContext() {
    ImportedPrivate.privateFunc() // expected-error {{module 'ImportedPrivate' has no member named 'privateFunc'}}
    ImportedPrivate.internalFunc() // expected-error {{module 'ImportedPrivate' has no member named 'internalFunc'}}
    ImportedPrivate.packageFunc()
    ImportedPrivate.publicFunc()

    ImportedInternal.privateFunc() // expected-error {{module 'ImportedInternal' has no member named 'privateFunc'}}
    ImportedInternal.internalFunc() // expected-error {{module 'ImportedInternal' has no member named 'internalFunc'}}
    ImportedInternal.packageFunc()
    ImportedInternal.publicFunc()

    ImportedPackage.privateFunc() // expected-error {{module 'ImportedPackage' has no member named 'privateFunc'}}
    ImportedPackage.internalFunc() // expected-error {{module 'ImportedPackage' has no member named 'internalFunc'}}
    ImportedPackage.packageFunc()
    ImportedPackage.publicFunc()

    ImportedPublic.privateFunc() // expected-error {{module 'ImportedPublic' has no member named 'privateFunc'}}
    ImportedPublic.internalFunc() // expected-error {{module 'ImportedPublic' has no member named 'internalFunc'}}
    ImportedPublic.packageFunc()
    ImportedPublic.publicFunc()
}

//--- OutOfPackageClient.swift

private import ImportedPrivate
internal import ImportedInternal
private import ImportedPackage
public import ImportedPublic // expected-warning{{public import of 'ImportedPublic' was not used in public declarations or inlinable code}}

ImportedPrivate.privateFunc() // expected-error {{module 'ImportedPrivate' has no member named 'privateFunc'}}
ImportedPrivate.internalFunc() // expected-error {{module 'ImportedPrivate' has no member named 'internalFunc'}}
ImportedPrivate.packageFunc() // expected-error {{module 'ImportedPrivate' has no member named 'packageFunc'}}
ImportedPrivate.publicFunc()

ImportedInternal.privateFunc() // expected-error {{module 'ImportedInternal' has no member named 'privateFunc'}}
ImportedInternal.internalFunc() // expected-error {{module 'ImportedInternal' has no member named 'internalFunc'}}
ImportedInternal.packageFunc() // expected-error {{module 'ImportedInternal' has no member named 'packageFunc'}}
ImportedInternal.publicFunc()

ImportedPackage.privateFunc() // expected-error {{module 'ImportedPackage' has no member named 'privateFunc'}}
ImportedPackage.internalFunc() // expected-error {{module 'ImportedPackage' has no member named 'internalFunc'}}
ImportedPackage.packageFunc() // expected-error {{module 'ImportedPackage' has no member named 'packageFunc'}}
ImportedPackage.publicFunc()

ImportedPublic.privateFunc() // expected-error {{module 'ImportedPublic' has no member named 'privateFunc'}}
ImportedPublic.internalFunc() // expected-error {{module 'ImportedPublic' has no member named 'internalFunc'}}
ImportedPublic.packageFunc() // expected-error {{module 'ImportedPublic' has no member named 'packageFunc'}}
ImportedPublic.publicFunc()

func funcContext() {
    ImportedPrivate.privateFunc() // expected-error {{module 'ImportedPrivate' has no member named 'privateFunc'}}
    ImportedPrivate.internalFunc() // expected-error {{module 'ImportedPrivate' has no member named 'internalFunc'}}
    ImportedPrivate.packageFunc() // expected-error {{module 'ImportedPrivate' has no member named 'packageFunc'}}
    ImportedPrivate.publicFunc()

    ImportedInternal.privateFunc() // expected-error {{module 'ImportedInternal' has no member named 'privateFunc'}}
    ImportedInternal.internalFunc() // expected-error {{module 'ImportedInternal' has no member named 'internalFunc'}}
    ImportedInternal.packageFunc() // expected-error {{module 'ImportedInternal' has no member named 'packageFunc'}}
    ImportedInternal.publicFunc()

    ImportedPackage.privateFunc() // expected-error {{module 'ImportedPackage' has no member named 'privateFunc'}}
    ImportedPackage.internalFunc() // expected-error {{module 'ImportedPackage' has no member named 'internalFunc'}}
    ImportedPackage.packageFunc() // expected-error {{module 'ImportedPackage' has no member named 'packageFunc'}}
    ImportedPackage.publicFunc()

    ImportedPublic.privateFunc() // expected-error {{module 'ImportedPublic' has no member named 'privateFunc'}}
    ImportedPublic.internalFunc() // expected-error {{module 'ImportedPublic' has no member named 'internalFunc'}}
    ImportedPublic.packageFunc() // expected-error {{module 'ImportedPublic' has no member named 'packageFunc'}}
    ImportedPublic.publicFunc()
}
