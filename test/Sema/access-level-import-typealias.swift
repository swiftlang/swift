// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-emit-module-interface(%t/Original.swiftinterface) %t/Original.swift
// RUN: %target-swift-typecheck-module-from-interface(%t/Original.swiftinterface)

// RUN: %target-swift-emit-module-interface(%t/Aliases.swiftinterface) %t/Aliases.swift -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Aliases.swiftinterface) -I %t

// RUN: %target-swift-frontend -typecheck -verify %t/UsesAliasesNoImport.swift -I %t \
// RUN:   -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -typecheck -verify %t/UsesAliasesNoImport.swift -I %t \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -enable-upcoming-feature InternalImportsByDefault

// RUN: %target-swift-frontend -emit-module %t/Original.swift -o %t \
// RUN:   -swift-version 6 -enable-library-evolution -package-name pkg

// RUN: %target-swift-frontend -emit-module %t/AliasesPkg.swift -o %t \
// RUN:   -swift-version 6 -enable-library-evolution -I %t -package-name pkg

// RUN: %target-swift-frontend -typecheck -verify %t/UsesAliasesPkg1.swift -I %t \
// RUN:   -swift-version 5 -enable-library-evolution -package-name pkg
// RUN: %target-swift-frontend -typecheck -verify %t/UsesAliasesPkg1.swift -I %t \
// RUN:   -swift-version 6 -enable-library-evolution -package-name pkg

// RUN: %target-swift-frontend -typecheck -verify %t/UsesAliasesPkg2.swift -I %t \
// RUN:   -swift-version 5 -enable-library-evolution -package-name pkg
// RUN: %target-swift-frontend -typecheck -verify %t/UsesAliasesPkg2.swift -I %t \
// RUN:   -swift-version 6 -enable-library-evolution -package-name pkg

//--- Original.swift
open class Clazz {}

//--- Aliases.swift
import Original
public typealias ClazzAlias = Clazz

//--- UsesAliasesNoImport.swift
public import Aliases
internal import Original // expected-note 2 {{class 'Clazz' imported as 'internal' from 'Original' here}}

// expected-error@+1 {{'ClazzAlias' aliases 'Original.Clazz' and cannot be used here because 'Original' was not imported publicly}}
public class InheritsFromClazzAlias: ClazzAlias {}

@inlinable public func inlinableFunc() {
  // expected-error@+1 {{'ClazzAlias' aliases 'Original.Clazz' and cannot be used in an '@inlinable' function because 'Original' was not imported publicly}}
  _ = ClazzAlias.self
}


//--- AliasesPkg.swift
public import Original // expected-warning {{public import of 'Original' was not used in public declarations or inlinable code}}
package typealias PkgClazzAlias = Clazz // expected-note 2 {{type alias 'PkgClazzAlias' is not '@usableFromInline' or public}}

//--- UsesAliasesPkg1.swift
public import AliasesPkg // expected-warning {{public import of 'AliasesPkg' was not used in public declarations or inlinable code}}
internal import Original // expected-note 1 {{class 'Clazz' imported as 'internal' from 'Original' here}}

// expected-error@+1 {{'PkgClazzAlias' aliases 'Original.Clazz' and cannot be used here because 'Original' was not imported publicly}}
package class InheritsFromPkgClazzAlias: PkgClazzAlias {}

@inlinable public func inlinableFunc() {
  // expected-error@+1 {{type alias 'PkgClazzAlias' is package and cannot be referenced from an '@inlinable' function}}
  _ = PkgClazzAlias.self
}

@inlinable package func inlinableFuncPkg() {
  // expected-error@+1 {{type alias 'PkgClazzAlias' is package and cannot be referenced from an '@inlinable' function}}
  _ = PkgClazzAlias.self
}


//--- UsesAliasesPkg2.swift
public import AliasesPkg // expected-warning {{public import of 'AliasesPkg' was not used in public declarations or inlinable code}}
package import Original

package class InheritsFromPkgClazzAlias: PkgClazzAlias {} // no-error

package func usePkgClazzAlias() {
  _ = PkgClazzAlias.self // no-error
}

@inlinable public func inlinableFunc() {
  // expected-error@+1 {{type alias 'PkgClazzAlias' is package and cannot be referenced from an '@inlinable' function}}
  _ = PkgClazzAlias.self
}

@inlinable package func inlinableFuncPkg() {
  // expected-error@+1 {{type alias 'PkgClazzAlias' is package and cannot be referenced from an '@inlinable' function}}
  _ = PkgClazzAlias.self
}

