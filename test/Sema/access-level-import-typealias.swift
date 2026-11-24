// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-emit-module-interface(%t/Original.swiftinterface) %t/Original.swift
// RUN: %target-swift-typecheck-module-from-interface(%t/Original.swiftinterface)

// RUN: %target-swift-emit-module-interface(%t/Aliases.swiftinterface) %t/Aliases.swift -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Aliases.swiftinterface) -I %t

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/UsesAliasesNoImport.swift -I %t \
// RUN:   -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/UsesAliasesNoImport.swift -I %t \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -enable-upcoming-feature InternalImportsByDefault

/// A type and a typealias to it with the same name, reached through different
/// imports, resolve to the most accessible of the two regardless of import order.
// RUN: %target-swift-emit-module-interface(%t/SameNameAlias.swiftinterface) %t/SameNameAlias.swift -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/SameNameAlias.swiftinterface) -I %t
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/PackageOriginalFirst.swift -I %t \
// RUN:   -package-name pkg -enable-upcoming-feature InternalImportsByDefault
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/PackageOriginalLast.swift -I %t \
// RUN:   -package-name pkg -enable-upcoming-feature InternalImportsByDefault
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/PublicOriginalFirst.swift -I %t \
// RUN:   -enable-upcoming-feature InternalImportsByDefault
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/PublicOriginalLast.swift -I %t \
// RUN:   -enable-upcoming-feature InternalImportsByDefault

// REQUIRES: swift_feature_InternalImportsByDefault

//--- Original.swift
open class Clazz {}

//--- Aliases.swift
import Original
public typealias ClazzAlias = Clazz

//--- UsesAliasesNoImport.swift
public import Aliases
internal import Original // expected-note 2 {{class 'Clazz' imported as 'internal' from 'Original' here}}

// expected-error@+1 {{'ClazzAlias' aliases 'Original.Clazz' and cannot be used in a public or '@usableFromInline' conformance because 'Original' was not imported publicly}}
public class InheritsFromClazzAlias: ClazzAlias {}

@inlinable public func inlinableFunc() {
  // expected-error@+1 {{'ClazzAlias' aliases 'Original.Clazz' and cannot be used in an '@inlinable' function because 'Original' was not imported publicly}}
  _ = ClazzAlias.self
}

//--- SameNameAlias.swift
public import Original
public typealias Clazz = Original.Clazz

//--- PackageOriginalFirst.swift
import Original
package import SameNameAlias

package func usesClazz(_: Clazz) {}
package let clazzArray: [Clazz] = []

//--- PackageOriginalLast.swift
package import SameNameAlias
import Original

package func usesClazz(_: Clazz) {}
package let clazzArray: [Clazz] = []

//--- PublicOriginalFirst.swift
internal import Original // expected-note {{class 'Clazz' imported as 'internal' from 'Original' here}}
public import SameNameAlias

// expected-error@+1 {{'Clazz' aliases 'Original.Clazz' and cannot be used here because 'Original' was not imported publicly}}
public func usesClazz(_: Clazz) {}

//--- PublicOriginalLast.swift
public import SameNameAlias
internal import Original // expected-note {{class 'Clazz' imported as 'internal' from 'Original' here}}

// expected-error@+1 {{'Clazz' aliases 'Original.Clazz' and cannot be used here because 'Original' was not imported publicly}}
public func usesClazz(_: Clazz) {}
