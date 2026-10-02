// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/emitted)
// RUN: %target-build-swift %s -module-name SetterAccess -package-name SetterAccessPackage -emit-module -emit-module-path %t/ -emit-symbol-graph -emit-symbol-graph-dir %t/emitted
// RUN: %empty-directory(%t/open)
// RUN: %target-swift-symbolgraph-extract -module-name SetterAccess -I %t -output-dir %t/open -minimum-access-level open
// RUN: %empty-directory(%t/public)
// RUN: %target-swift-symbolgraph-extract -module-name SetterAccess -I %t -output-dir %t/public
// RUN: %empty-directory(%t/package)
// RUN: %target-swift-symbolgraph-extract -module-name SetterAccess -I %t -output-dir %t/package -minimum-access-level package
// RUN: %empty-directory(%t/internal)
// RUN: %target-swift-symbolgraph-extract -module-name SetterAccess -I %t -output-dir %t/internal -minimum-access-level internal
// RUN: %empty-directory(%t/private)
// RUN: %target-swift-symbolgraph-extract -module-name SetterAccess -I %t -output-dir %t/private -minimum-access-level private

// Declarations only show the setters that are accessible at the graph's
// minimum access level, since they don't include modifiers like
// `internal(set)`. HIDE-<ACCESS> and SHOW-<ACCESS> check the declarations of
// properties with setters of that access level. Open graphs show public setters.

// RUN: %{python} %S/../Inputs/print_declarations.py %t/open/SetterAccess.symbols.json | %FileCheck %s --match-full-lines --check-prefix=OPEN
// RUN: %{python} %S/../Inputs/print_declarations.py %t/public/SetterAccess.symbols.json | %FileCheck %s --match-full-lines --check-prefixes=ALL,HIDE-PACKAGE,HIDE-INTERNAL,HIDE-PRIVATE
// RUN: %{python} %S/../Inputs/print_declarations.py %t/emitted/SetterAccess.symbols.json | %FileCheck %s --match-full-lines --check-prefixes=ALL,HIDE-PACKAGE,HIDE-INTERNAL,HIDE-PRIVATE
// RUN: %{python} %S/../Inputs/print_declarations.py %t/package/SetterAccess.symbols.json | %FileCheck %s --match-full-lines --check-prefixes=ALL,SHOW-PACKAGE,HIDE-INTERNAL,HIDE-PRIVATE
// RUN: %{python} %S/../Inputs/print_declarations.py %t/internal/SetterAccess.symbols.json | %FileCheck %s --match-full-lines --check-prefixes=ALL,SHOW-PACKAGE,SHOW-INTERNAL,HIDE-PRIVATE
// RUN: %{python} %S/../Inputs/print_declarations.py %t/private/SetterAccess.symbols.json | %FileCheck %s --match-full-lines --check-prefixes=ALL,SHOW-PACKAGE,SHOW-INTERNAL,SHOW-PRIVATE

@propertyWrapper
public struct Wrapper {
  public internal(set) var wrappedValue: Int
  public init(wrappedValue: Int) { self.wrappedValue = wrappedValue }
}

@propertyWrapper
public struct ReadOnly {
  public var wrappedValue: Int { 0 }
  public init(wrappedValue: Int) {}
}

public struct S {
  // https://github.com/swiftlang/swift/issues/92083
  // The wrapper's internal setter is accessible here, so this property has a
  // public setter.
  @Wrapper public var wrapped = 0
  // ALL-DAG: S.wrapped: @Wrapper var wrapped: Int { get set }

  @Wrapper public internal(set) var wrappedInternal = 0
  // HIDE-INTERNAL-DAG: S.wrappedInternal: @Wrapper var wrappedInternal: Int { get }
  // SHOW-INTERNAL-DAG: S.wrappedInternal: @Wrapper var wrappedInternal: Int { get set }

  @ReadOnly public var readOnly = 0
  // ALL-DAG: S.readOnly: @ReadOnly var readOnly: Int { get }

  public internal(set) var storedInternal = 0
  // HIDE-INTERNAL-DAG: S.storedInternal: var storedInternal: Int { get }
  // SHOW-INTERNAL-DAG: S.storedInternal: var storedInternal: Int

  public internal(set) var observedInternal = 0 { didSet {} }
  // HIDE-INTERNAL-DAG: S.observedInternal: var observedInternal: Int { get }
  // SHOW-INTERNAL-DAG: S.observedInternal: var observedInternal: Int { get set }

  public internal(set) lazy var lazyInternal = 0
  // HIDE-INTERNAL-DAG: S.lazyInternal: lazy var lazyInternal: Int { mutating get }
  // SHOW-INTERNAL-DAG: S.lazyInternal: lazy var lazyInternal: Int { mutating get set }

  public package(set) var computedPackage: Int {
    get { 0 }
    set {}
  }
  // HIDE-PACKAGE-DAG: S.computedPackage: var computedPackage: Int { get }
  // SHOW-PACKAGE-DAG: S.computedPackage: var computedPackage: Int { get set }

  public internal(set) var computedInternal: Int {
    get { 0 }
    set {}
  }
  // HIDE-INTERNAL-DAG: S.computedInternal: var computedInternal: Int { get }
  // SHOW-INTERNAL-DAG: S.computedInternal: var computedInternal: Int { get set }

  public private(set) var computedPrivate: Int {
    get { 0 }
    nonmutating set {}
  }
  // HIDE-PRIVATE-DAG: S.computedPrivate: var computedPrivate: Int { get }
  // SHOW-PRIVATE-DAG: S.computedPrivate: var computedPrivate: Int { get nonmutating set }

  public var nonmutatingSetter: Int {
    get { 0 }
    nonmutating set {}
  }
  // ALL-DAG: S.nonmutatingSetter: var nonmutatingSetter: Int { get nonmutating set }

  public internal(set) var mutatingGetter: Int {
    mutating get { 0 }
    set {}
  }
  // HIDE-INTERNAL-DAG: S.mutatingGetter: var mutatingGetter: Int { mutating get }
  // SHOW-INTERNAL-DAG: S.mutatingGetter: var mutatingGetter: Int { mutating get set }

  public internal(set) subscript(index: Int) -> Int {
    get { 0 }
    set {}
  }
  // HIDE-INTERNAL-DAG: S.subscript(_:): subscript(index: Int) -> Int { get }
  // SHOW-INTERNAL-DAG: S.subscript(_:): subscript(index: Int) -> Int { get set }

  public subscript(index: Bool) -> Int {
    get { 0 }
    set {}
  }
  // ALL-DAG: S.subscript(_:): subscript(index: Bool) -> Int { get set }

  // A setter as accessible as its property is shown, even if the property is
  // only included because of its documentation visibility.
  @_documentation(visibility: public)
  internal var documentedStored = 0
  // ALL-DAG: S.documentedStored: var documentedStored: Int

  @_documentation(visibility: public)
  internal var documentedComputed: Int {
    get { 0 }
    set {}
  }
  // ALL-DAG: S.documentedComputed: var documentedComputed: Int { get set }

  @_documentation(visibility: public)
  internal private(set) var documentedPrivate: Int {
    get { 0 }
    set {}
  }
  // HIDE-PRIVATE-DAG: S.documentedPrivate: var documentedPrivate: Int { get }
  // SHOW-PRIVATE-DAG: S.documentedPrivate: var documentedPrivate: Int { get set }
}

public protocol P {
  var requirement: Int { get set }
  // ALL-DAG: P.requirement: var requirement: Int { get set }
}

open class C {
  open public(set) var publicSetter: Int {
    get { 0 }
    set {}
  }
  // OPEN-DAG: C.publicSetter: var publicSetter: Int { get set }
  // ALL-DAG: C.publicSetter: var publicSetter: Int { get set }

  open public(set) var storedPublicSetter = 0
  // OPEN-DAG: C.storedPublicSetter: var storedPublicSetter: Int
  // ALL-DAG: C.storedPublicSetter: var storedPublicSetter: Int

  open internal(set) var internalSetter: Int {
    get { 0 }
    set {}
  }
  // OPEN-DAG: C.internalSetter: var internalSetter: Int { get }
  // HIDE-INTERNAL-DAG: C.internalSetter: var internalSetter: Int { get }
  // SHOW-INTERNAL-DAG: C.internalSetter: var internalSetter: Int { get set }
}
