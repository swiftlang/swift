// Recover from missing types hidden behind an importation-only when indexing
// a system module.
// rdar://problem/52837313

// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

//// Build the private module, the public module and the client app normally.
//// Force the public module to be system with an underlying Clang module.
// RUN: %target-swift-frontend -emit-module %t/PrivateLib.swift \
// RUN:   -emit-module-path %t/PrivateLib.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/PublicLib.swift \
// RUN:   -emit-module-path %t/PublicLib.swiftmodule \
// RUN:   -I %t -import-underlying-module

//// Remove the private module make sure it's not somehow loaded.
// RUN: rm %t/PrivateLib.swiftmodule

//// The client app should build and index OK without the private module.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -index-system-modules -index-store-path %t
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -D FAIL_TYPECHECK -verify
// RUN: %target-swift-frontend -emit-sil %t/Client.swift -I %t \
// RUN:   -module-name client

//// Printing the public module should not crash when checking for overrides of
//// methods from the private module.
// RUN: %target-swift-ide-test -print-module -module-to-print=PublicLib \
// RUN:   -source-filename=x -skip-overrides -I %t

//--- module.modulemap
module PublicLib [system] {}

//--- PrivateLib.swift

@propertyWrapper
public struct IoiPropertyWrapper<V> {
  var content: V

  public init(_ v: V) {
    content = v
  }

  public var wrappedValue: V {
    return content
  }
}

public struct HiddenGenStruct<A: HiddenProtocol> {
  public init() {}
}

public protocol HiddenProtocol {
  associatedtype Value
}

public protocol HiddenProtocol2 {}

public protocol HiddenProtocolWithOverride {
  func hiddenOverride()
}

open class HiddenClass {}

public struct HiddenRawType: ExpressibleByStringLiteral, Equatable, CustomStringConvertible {

    fileprivate var staticValue: String

    public init(stringLiteral value: String) {
        self.init(value)
    }

    public init(_ value: String) {
        self.staticValue = value
    }

    public static func == (lhs: HiddenRawType, rhs: HiddenRawType) -> Bool {
        return lhs.staticValue == rhs.staticValue
    }

    public var description: String { self.staticValue }
}

//--- PublicLib.swift

@_implementationOnly import PrivateLib

struct LibProtocolConstraint { }

protocol LibProtocolTABound { }
struct LibProtocolTA: LibProtocolTABound { }

protocol LibProtocol {
  associatedtype TA: LibProtocolTABound = LibProtocolTA

  func hiddenRequirement<A>(
      param: HiddenGenStruct<A>
  ) where A.Value == LibProtocolConstraint
}

extension LibProtocol where TA == LibProtocolTA {
  func hiddenRequirement<A>(
      param: HiddenGenStruct<A>
  ) where A.Value == LibProtocolConstraint { }
}

public struct PublicStruct: LibProtocol {
  typealias TA = LibProtocolTA

  public init() { }

  public var nonWrappedVar: String = "some text"

  @IoiPropertyWrapper("some text")
  private var wrappedVar: String
}

struct StructWithOverride: HiddenProtocolWithOverride {
  func hiddenOverride() {}
}

internal protocol RefinesHiddenProtocol: HiddenProtocol {

}

public struct PublicStructConformsToHiddenProtocol: RefinesHiddenProtocol {
  public typealias Value = Int

  public init() { }
}

public class SomeClass {
    func funcUsingIoiType(_ a: HiddenClass) {}
}

// Check that we recover from a reference to an implementation-only
// imported type in a protocol composition. rdar://78631465
protocol CompositionMemberInheriting : HiddenProtocol2 {}
protocol CompositionMemberSimple {}
protocol InheritingFromComposition : CompositionMemberInheriting & CompositionMemberSimple {}
struct StructInheritingFromComposition : CompositionMemberInheriting & CompositionMemberSimple {}
class ClassInheritingFromComposition : CompositionMemberInheriting & CompositionMemberSimple {}
protocol InheritingFromCompositionDirect : CompositionMemberSimple & HiddenProtocol2 {}

// rdar://147091863
enum InternalEnumWithRawType: HiddenRawType {
    case a
}

class InternalSubclass: HiddenClass {}
class GenericBase<T> {}
class Sub: GenericBase<Sub.Nested> {
  class Nested: HiddenClass {}
}

func InternalFuncWithParam(a: HiddenRawType)  {}

//--- Client.swift

import PublicLib

var s = PublicStruct()
print(s.nonWrappedVar)

var p = PublicStructConformsToHiddenProtocol()
print(p)

#if FAIL_TYPECHECK
    // Access to a missing member on an AnyObject triggers a typo correction
    // that looks at *all* class members. rdar://79427805
    class ClassUnrelatedToSomeClass {}
    var something = ClassUnrelatedToSomeClass() as AnyObject
    something.triggerTypoCorrection = 123 // expected-error {{value of type 'AnyObject' has no member 'triggerTypoCorrection'}}
#endif
