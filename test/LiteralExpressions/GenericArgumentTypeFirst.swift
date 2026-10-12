// REQUIRES: swift_feature_LiteralExpressions

// A parenthesized generic argument is a type if it can be interpreted as one,
// and is type-checked as an 'Int' literal expression otherwise (SE-0531). The
// type interpretation is built from the argument's syntax alone, without
// lookup, and is resolved in the same type resolution as the unparenthesized
// spelling.
// RUN: %target-typecheck-verify-swift -swift-version 5 -target %target-swift-6.2-abi-triple -enable-experimental-feature LiteralExpressions -verify-additional-file Swift.Cell

protocol Container<T> { associatedtype T }
protocol Base { associatedtype Inner: Deep }
protocol Deep { associatedtype Deeper }
protocol P1 {}
protocol P2 {}
protocol P3 {}
struct Box<T> {}
struct Vec<let N: Int, T> {}
struct Limits { static let count = 4 }
struct E: Error {}

// =============================================================================
// Protocol requirements
//
// A protocol's requirement signature resolves these arguments at the structural
// stage. Interpreting them by value lookup needed that requirement signature, so
// most of these used to be circular references.
// =============================================================================

protocol Requirements {
  associatedtype A: Base
  associatedtype B: Hashable

  associatedtype Name: Container<(A)>
  associatedtype ArrayOf: Container<([A])>
  associatedtype DictionaryOf: Container<([B: A])>
  associatedtype OptionalOf: Container<(A?)>
  associatedtype Function: Container<((A) -> B)>
  associatedtype Tuple: Container<(A, B)>
  associatedtype NestedTuple: Container<((A, Self))>
  associatedtype Metatype: Container<(A.Type)>
  associatedtype Member: Container<(A.Inner)>
  associatedtype MemberMetatype: Container<(A.Inner.Type)>
  associatedtype ArrayOfMember: Container<([A.Inner])>
  associatedtype MemberChain: Container<(A.Inner.Deeper)>
  associatedtype MemberInTuple: Container<(A.Inner, Self)>
  associatedtype SelfMember: Container<(Self.A)>
  associatedtype SelfType: Container<(Self)>
  associatedtype Composition: Container<(P1 & P2)>
  associatedtype Empty: Container<()>
  associatedtype ProtocolMetatype: Container<(P1.Protocol)>
  associatedtype Existential: Container<(any P1)>
  associatedtype ModuleQualified: Container<(Swift.Int)>
  associatedtype ModuleQualifiedChain: Container<(Swift.Int.Magnitude)>
  associatedtype ConcreteMember: Container<(Int.Magnitude)>
  associatedtype Specialization: Container<(Box<A>)>
  associatedtype SpecializedMember: Container<(Box<A.Inner>)>
  associatedtype ValueSpecialization: Container<(Vec<3, Int>)>
  associatedtype MemberOfSpecialization: Container<(Array<A>.Element)>

  typealias Alias = A
  associatedtype AliasRootedMember: Container<(Alias.Inner)>
  typealias InTypealias = Container<(A)>
  func inWhereClause<T>(_: T) where T: Container<(A)>
}

protocol InheritedMember: Base {
  associatedtype Unqualified: Container<(Inner)>
  associatedtype Qualified: Container<(Self.Inner)>
}

protocol ConcreteAliasMember {
  typealias C = Int
  associatedtype B: Container<(C.Magnitude)>
}

// Each argument is interpreted as the type its unparenthesized spelling names.
func requirementInterpretations<X: Requirements>(_: X) {
  let _: Box<X.Name.T> = Box<X.A>()
  let _: Box<X.ArrayOf.T> = Box<[X.A]>()
  let _: Box<X.DictionaryOf.T> = Box<[X.B: X.A]>()
  let _: Box<X.OptionalOf.T> = Box<X.A?>()
  let _: Box<X.Function.T> = Box<(X.A) -> X.B>()
  let _: Box<X.Tuple.T> = Box<(X.A, X.B)>()
  let _: Box<X.Metatype.T> = Box<X.A.Type>()
  let _: Box<X.Member.T> = Box<X.A.Inner>()
  let _: Box<X.MemberChain.T> = Box<X.A.Inner.Deeper>()
  let _: Box<X.Composition.T> = Box<any P1 & P2>()
  let _: Box<X.Empty.T> = Box<()>()
  let _: Box<X.Specialization.T> = Box<Box<X.A>>()
  let _: Box<X.MemberOfSpecialization.T> = Box<X.A>()
}

// The generic signature of any declaration is computed at the structural stage
// too, not only a protocol's requirement signature.
func dependentMemberInFunction<U: Base>(_: U, _: Box<(U.Inner)>) {}
struct DependentMemberInWhereClause<T: Base, U: Container>
  where U.T == Box<(T.Inner)> {}
struct OuterDependentMember<U: Base> {
  func f<V>(_: V, _: Box<(U.Inner)>) {}
}

// =============================================================================
// '&' and '->'
//
// A sequence is interpreted by operator spelling, as the type parser parses it,
// so looking up a protocol's own '&' cannot cycle. '&' binds more tightly than
// '->', and '->' associates to the right.
// =============================================================================

protocol DeclaresAmpersand {
  static func & (lhs: Self, rhs: Self) -> Self
  associatedtype Two: Container<(P1 & P2)>
  associatedtype Three: Container<(P1 & P2 & P3)>
}

protocol RefinesBinaryInteger: BinaryInteger {
  associatedtype Two: Container<(P1 & P2)>
}

struct Out {}

typealias CompositionInput = Box<(P1 & P2 -> Out)>
// expected-error@-1 {{single argument function types require parentheses}} {{35-35=(}} {{42-42=)}}
typealias ParenthesizedCompositionInput = Box<((P1 & P2) -> Out)>
typealias RightAssociative = Box<((P1) -> (P2) -> Out)>
typealias CompositionInResultInput = Box<((P1) -> P2 & P3 -> Out)>
// expected-error@-1 {{single argument function types require parentheses}} {{51-51=(}} {{58-58=)}}
typealias Effects = Box<((P1) async throws -> (P2) throws(E) -> Out)>
typealias NoInput = Box<(() -> Out)>

func sequenceInterpretations() {
  let a: CompositionInput? = nil
  let _: Box<(P1 & P2) -> Out>? = a
  let b: ParenthesizedCompositionInput? = nil
  let _: Box<(P1 & P2) -> Out>? = b
  let c: RightAssociative? = nil
  let _: Box<(P1) -> ((P2) -> Out)>? = c
  let d: CompositionInResultInput? = nil
  let _: Box<(P1) -> ((P2 & P3) -> Out)>? = d
  let e: Effects? = nil
  let _: Box<(P1) async throws -> ((P2) throws(E) -> Out)>? = e
  let f: NoInput? = nil
  let _: Box<() -> Out>? = f
}

// Like the type parser, a composition is located at its first '&', so a
// diagnostic about the whole composition points there in both spellings.
struct NoncopyableContainer<T: ~Copyable> {}
typealias CompositionLocation = NoncopyableContainer<Copyable & ~Copyable>
// expected-error@-1:63 {{composition cannot contain '~Copyable' when another member requires 'Copyable'}}
typealias ParenCompositionLocation = NoncopyableContainer<(Copyable & ~Copyable)>
// expected-error@-1:69 {{composition cannot contain '~Copyable' when another member requires 'Copyable'}}

// =============================================================================
// Syntax with no literal-expression interpretation
//
// Optionals, collections, tuples, function types, metatypes and generic
// argument lists cannot be literal expressions, so such an argument is resolved
// as a type, and diagnosed as one.
// =============================================================================

struct NotAProtocol {}

typealias OptionalOfMissing = Box<(Undefined?)>
// expected-error@-1 {{cannot find type 'Undefined' in scope}}
typealias ArrayOfMissing = Box<([Undefined])>
// expected-error@-1 {{cannot find type 'Undefined' in scope}}
typealias DictionaryOfMissing = Box<([Int: Undefined])>
// expected-error@-1 {{cannot find type 'Undefined' in scope}}
typealias TupleOfMissing = Box<(Int, Undefined)>
// expected-error@-1 {{cannot find type 'Undefined' in scope}}
typealias LabeledElement = Box<(x: Int)>
// expected-error@-1 {{cannot create a single-element tuple with an element label}}
typealias FunctionOfMissing = Box<((Undefined) -> Int)>
// expected-error@-1 {{cannot find type 'Undefined' in scope}}
typealias SpecializationOfMissing = Box<(Box<Undefined>)>
// expected-error@-1 {{cannot find type 'Undefined' in scope}}
typealias MetatypeOfMissing = Box<(Undefined.Type)>
// expected-error@-1 {{cannot find type 'Undefined' in scope}}
typealias ProtocolMetatypeOfNonProtocol = Box<(NotAProtocol.Protocol)>
// expected-error@-1 {{cannot use 'Protocol' with non-protocol type 'NotAProtocol'}}
typealias ArrayOfValue = Box<([Limits.count])>
// expected-error@-1 {{static property 'count' is not a member type of 'Limits'}}
typealias OptionalOfValue = Box<(Limits.count?)>
// expected-error@-1 {{static property 'count' is not a member type of 'Limits'}}

protocol TypeOnlyRequirements {
  associatedtype A
  associatedtype B: Container<(A.Protocol)>
  // expected-error@-1 {{cannot use 'Protocol' with non-protocol type 'Self.A'}}
  associatedtype C: Container<([Limits.count])>
  // expected-error@-1 {{static property 'count' is not a member type of 'Limits'}}
}

func placeholders() {
  let _: Box<(_)> = Box<Int>()
  let _: Box<(Int, _)> = Box<(Int, String)>()
}

struct Variadic<each T> {
  typealias Expansion = Box<(repeat each T)>
  typealias ExpansionInTuple = Box<(Int, repeat each T)>
}

// =============================================================================
// Syntax with both interpretations
//
// Names, member references, parentheses, '&' and prefix '~' can be either. Such
// an argument is a type if it resolves as one, and a value otherwise.
// =============================================================================

enum InvaderGrid { static let count = 33 }

typealias ParenName = Box<(Int)>
typealias NestedParens = Box<((Int))>
typealias ParenComposition = Box<(P1 & P2)>
typealias ParenModuleQualified = Box<(Swift.Int)>
typealias ParenConcreteMember = Box<(Int.Magnitude)>
typealias ParenInverse = Box<(~Copyable)>
// expected-error@-1 {{type '~Copyable' does not conform to protocol 'Copyable'}}

typealias StaticMember = Vec<(InvaderGrid.count), Int>
typealias BitwiseAnd = Vec<(6 & 5), Int>
typealias NestedParenValue = Vec<((3)), Int>

func overlapInterpretations() {
  let _: Box<Int> = ParenName()
  let _: Box<Int> = NestedParens()
  let _: Box<UInt> = ParenConcreteMember()
  let _: Vec<33, Int> = StaticMember()
  let _: Vec<4, Int> = BitwiseAnd()
  let _: Vec<3, Int> = NestedParenValue()
}

// A value does not hide a type of the same name (SE-0531).
func shadowedByLocal() {
  let Int = 0
  let a: Array<(Int)> = []
  _ = (Int, a)
}

enum Count {}
struct ShadowedByValue {
  static let Count = 4
  typealias A = Vec<(Count), Int>
  // expected-error@-1 {{cannot pass type 'Count' as a value for generic value 'N'}}
  typealias B = Box<(Count)>
}

// A name that is only a value is type-checked as one.
struct ValueInExpression {
  static let Count = 4
  typealias A = Vec<(Count + 1), Int>
  func f() { let _: A = Vec<5, Int>() }
}

// A value generic parameter is interpreted as a type, which resolves to its
// value. The value path type-checks the expression as written.
struct ValueGenericParameter<let N: Int> {
  typealias Paren = Vec<(N), Int>
  func f() { let _: Paren = Vec<N, Int>() }

  typealias Member = Vec<(N.magnitude), Int>
  // expected-error@-1 {{cannot convert value of type 'UInt' to raw type 'Int'}}
  // expected-error@-2 {{generic value must be an integer literal expression}}
}

// A missing member of a module is diagnosed against the module.
typealias ModuleMissingMember = Box<(Swift.NoSuchType)>
// expected-error@-1 {{module 'Swift' has no member named 'NoSuchType'}}
// expected-error@-2 {{generic value must be an integer literal expression}}

// Other dotted names that do not resolve as types are type-checked as values
// too, and the value path's diagnostics follow.
struct OuterWithPrivate {
  private struct Hidden {} // expected-note {{'Hidden' declared here}}
}
typealias PrivateMember = Box<(OuterWithPrivate.Hidden)>
// expected-error@-1 {{'Hidden' is inaccessible due to 'private' protection level}}
// expected-error@-2 {{cannot convert value of type 'OuterWithPrivate.Hidden.Type' to raw type 'Int'}}
// expected-error@-3 {{generic value must be an integer literal expression}}

protocol HasB { associatedtype B }
struct ExistentialMember<T> where T == any HasB {
  // expected-warning@-1 {{same-type requirement makes generic parameter 'T' non-generic}}
  var member: Box<(T.B)>
  // expected-error@-1 {{cannot access associated type 'B' from 'any HasB'; use a concrete type or generic parameter base instead}}
  // expected-error@-2 {{cannot convert value of type '_.Type' to raw type 'Int'}}
  // expected-error@-3 {{generic value must be an integer literal expression}}
}

// A member of a specialized base can be either a type or a value.
typealias GenericAlias<T> = Limits
struct Nest<T> { struct Inner {} }
struct Sized<let N: Int> { struct Inner {} }
let sizedCount = 3

typealias MemberOfSpecializedAlias = Vec<(GenericAlias<Int>.count), Int>
typealias MemberOfParenthesizedSpecializedAlias = Vec<((GenericAlias<Int>).count), Int>
typealias MemberTypeOfSpecialization = Box<(Nest<Int>.Inner)>
typealias MemberTypeOfValueSpecialization = Box<(Sized<(sizedCount)>.Inner)>

func specializedBaseInterpretations() {
  let _: Vec<4, Int> = MemberOfSpecializedAlias()
  let _: Vec<4, Int> = MemberOfParenthesizedSpecializedAlias()
  let _: Box<Nest<Int>.Inner> = MemberTypeOfSpecialization()
  let _: Box<Sized<3>.Inner> = MemberTypeOfValueSpecialization()
}

// An error in a specialized base's generic arguments is reported once, and the
// value path then reports the argument too.
typealias MissingInSpecializedAlias = Vec<(GenericAlias<Undefined>.count), Int>
// expected-error@-1 {{cannot find type 'Undefined' in scope}}
// expected-error@-2 {{generic value must be an integer literal expression}}
typealias MissingInValueSpecialization = Box<(Sized<(Undefined)>.Inner)>
// expected-error@-1 {{cannot find 'Undefined' in scope}}
// expected-error@-2 2 {{generic value must be an integer literal expression}}

// Problems found while applying a specialized base's generic arguments are
// reported once, and then the value path reports the argument.
protocol NoPrimaryAssociatedTypes {} // expected-note {{'NoPrimaryAssociatedTypes' declared here}}
struct NumericOnly<T: Numeric> { struct Inner {} }

typealias NoPrimaryAssociatedTypesBase = Vec<(NoPrimaryAssociatedTypes<Int>.count), Int>
// expected-error@-1 {{protocol 'NoPrimaryAssociatedTypes' does not have primary associated types that can be constrained}}
// expected-error@-2 {{generic value must be an integer literal expression}}
typealias UnmetRequirementBase = Box<(NumericOnly<String>.Inner)>
// expected-error@-1 {{type 'String' does not conform to protocol 'Numeric'}}
// expected-error@-2 {{generic value must be an integer literal expression}}
typealias ReservedProtocolBase = Vec<(GenericAlias<Deinitable>.count), Int>
// expected-error@-1 {{'Deinitable' is reserved for use by the compiler}}
// expected-error@-2 {{generic value must be an integer literal expression}}

// Problems reported while still forming the type are not lost by trying the
// type interpretation silently.
struct NotHashable {}

typealias IUOInBase = Box<Nest<Int!>.Inner>
// expected-error@-1 {{using '!' is not allowed here}}
// expected-note@-2 {{use '?' instead}}
typealias ParenIUOInBase = Box<(Nest<Int!>.Inner)>
// expected-error@-1 {{using '!' is not allowed here}}
// expected-note@-2 {{use '?' instead}}
// expected-error@-3 {{to raw type 'Int'}}
// expected-error@-4 {{generic value must be an integer literal expression}}
typealias ThrowsNonErrorInBase = Box<Nest<() throws(Int) -> Void>.Inner>
// expected-error@-1 {{thrown type 'Int' does not conform to the 'Error' protocol}}
typealias ParenThrowsNonErrorInBase = Box<(Nest<() throws(Int) -> Void>.Inner)>
// expected-error@-1 {{thrown type 'Int' does not conform to the 'Error' protocol}}
// expected-error@-2 {{to raw type 'Int'}}
// expected-error@-3 {{generic value must be an integer literal expression}}
typealias UnhashableKeyInBase = Box<Nest<[NotHashable: Int]>.Inner>
// expected-error@-1 {{type 'NotHashable' does not conform to protocol 'Hashable'}}
typealias ParenUnhashableKeyInBase = Box<(Nest<[NotHashable: Int]>.Inner)>
// expected-error@-1 {{type 'NotHashable' does not conform to protocol 'Hashable'}}
// expected-error@-2 {{generic value must be an integer literal expression}}

// A type interpretation is resolved once. A constraint system opens its
// generic types once, and a warning is emitted once.
func resolvedOnceInExpressions() {
  let _: Box<[Int]> = Box<(Array)>()
  let _: Box<Set<Int>> = Box<(Set)>()
  let _: Box<Nest<Int>.Inner> = Box<(Nest<_>.Inner)>()
}

typealias VoidPointee = Box<(UnsafePointer<Void>.Pointee)>
// expected-warning@-1 {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}

// =============================================================================
// Names that type lookup finds
//
// A lone name that type lookup finds is a type, even if it does not resolve as
// one here, and is diagnosed as the unparenthesized spelling is. So is a
// composition or an inverse of such names.
// =============================================================================

struct Generic<T> {} // expected-note {{generic struct 'Generic' declared here}}
typealias UnboundGeneric = Box<(Generic)>
// expected-error@-1 {{reference to generic type 'Generic' requires arguments in <...>}}

struct Outer<T> {}
extension Outer where T == Int { struct Nested {} }
// expected-note@-1 {{requirement specified as 'T' == 'Int' [with T = T]}}
extension Outer {
  typealias UnmetContextualRequirement = Box<(Nested)>
  // expected-error@-1 {{'Outer<T>.Nested' requires the types 'T' and 'Int' be equivalent}}
}

struct PackWithoutEach<each T> {
  typealias A = Box<(T)>
  // expected-error@-1 {{pack reference 'T' requires expansion using keyword 'repeat'}}
  // expected-error@-2 {{type pack 'T' must be referenced with 'each'}}
}

// Type lookup finds a module by its name.
typealias ModuleName = Box<(Swift)>
// expected-error@-1 {{cannot find type 'Swift' in scope}}
// expected-note@-2 {{cannot use module 'Swift' as a type}}

// The standard library's 'Cell' requires an experimental feature.
// expected-note@Swift.Cell:2 2 {{generic struct 'Cell' declared here}}
typealias ExperimentalStdlibType = Box<(Cell)>
// expected-error@-1 {{using 'Cell' requires an experimental feature; use '-enable-experimental-feature Cells'}}
// expected-error@-2 {{reference to generic type 'Cell' requires arguments in <...>}}
typealias ExperimentalStdlibComposition = Box<(Cell & P1)>
// expected-error@-1 {{using 'Cell' requires an experimental feature; use '-enable-experimental-feature Cells'}}
// expected-error@-2 {{reference to generic type 'Cell' requires arguments in <...>}}

class ClassA {}
class ClassB {}

typealias CompositionWithStruct = Box<Int & P1>
// expected-error@-1 {{non-protocol, non-class type 'Int' cannot be used within a protocol-constrained type}}
typealias ParenCompositionWithStruct = Box<(Int & P1)>
// expected-error@-1 {{non-protocol, non-class type 'Int' cannot be used within a protocol-constrained type}}
typealias TwoClasses = Box<ClassA & ClassB>
// expected-error@-1 {{protocol-constrained type cannot contain class 'ClassB' because it already contains class 'ClassA'}}
typealias ParenTwoClasses = Box<(ClassA & ClassB)>
// expected-error@-1 {{protocol-constrained type cannot contain class 'ClassB' because it already contains class 'ClassA'}}
typealias InverseOfProtocol = Box<~P1>
// expected-error@-1 {{type 'P1' cannot be suppressed}}
typealias ParenInverseOfProtocol = Box<(~P1)>
// expected-error@-1 {{type 'P1' cannot be suppressed}}

struct NoncopyableBox<T: ~Copyable> {}
typealias CopyableAndNot = NoncopyableBox<Copyable & ~Copyable>
// expected-error@-1 {{composition cannot contain '~Copyable' when another member requires 'Copyable'}}
typealias ParenCopyableAndNot = NoncopyableBox<(Copyable & ~Copyable)>
// expected-error@-1 {{composition cannot contain '~Copyable' when another member requires 'Copyable'}}
typealias AnyObjectAndNot = NoncopyableBox<AnyObject & ~Copyable>
// expected-error@-1 {{composition involving 'AnyObject' cannot contain '~Copyable'}}
typealias ParenAnyObjectAndNot = NoncopyableBox<(AnyObject & ~Copyable)>
// expected-error@-1 {{composition involving 'AnyObject' cannot contain '~Copyable'}}

// 'Self' names a type inside a type.
struct SelfComposition {
  var bare: Box<Self & P1>
  // expected-error@-1 {{non-protocol, non-class type 'SelfComposition' cannot be used within a protocol-constrained type}}
  var paren: Box<(Self & P1)>
  // expected-error@-1 {{non-protocol, non-class type 'SelfComposition' cannot be used within a protocol-constrained type}}
}

class SelfInParameter {
  func bare(_: Box<Self>) {}
  // expected-error@-1 {{covariant 'Self' or 'Self?' can only appear as the type of a property, subscript or method result; did you mean 'SelfInParameter'?}}
  func paren(_: Box<(Self)>) {}
  // expected-error@-1 {{covariant 'Self' or 'Self?' can only appear as the type of a property, subscript or method result; did you mean 'SelfInParameter'?}}
}

// =============================================================================
// Abandoned type interpretations
//
// Building a type interpretation can diagnose. If the argument turns out to
// have no type interpretation, those diagnostics are dropped, and only the
// value path's own are emitted.
// =============================================================================

struct In {}

typealias TupleWithValue = Box<((In -> Out), 3)>
// expected-error@-1 {{single argument function types require parentheses}}
// expected-error@-2 {{cannot convert value of type '(((In) -> Out).Type, Int)' to raw type 'Int'}}
// expected-error@-3 {{generic value must be an integer literal expression}}
typealias SumWithType = Vec<((In -> Out) + 1), Int>
// expected-error@-1 {{single argument function types require parentheses}}
// expected-error@-2 {{cannot convert value of type '((In) -> Out).Type' to expected argument type 'Int'}}
// expected-error@-3 {{generic value must be an integer literal expression}}

// =============================================================================
// Known limitations
//
// An argument with no type interpretation is type-checked as a value. At the
// structural stage, a value reference that needs the generic signature being
// computed is still a circular reference.
// =============================================================================

protocol PostfixOperatorMember { // expected-error 2 {{circular reference}}
  associatedtype A: Base
  associatedtype B: Container<(A.+)>
  // expected-error@-1 {{cannot find operator '.+' in scope}}
  // expected-error@-2 {{generic value must be an integer literal expression}}
  // expected-note@-3 2 {{while resolving type 'Container<(A.+)>'}}
  // expected-note@-4 2 {{while resolving type '(A.+)'}}
}

protocol HasF { static func f() -> Int }
protocol StaticCall { // expected-error 2 {{circular reference}}
  associatedtype A: HasF
  associatedtype B: Container<(A.f())>
  // expected-error@-1 {{generic value must be an integer literal expression}}
  // expected-note@-2 2 {{while resolving type 'Container<(A.f())>'}}
  // expected-note@-3 2 {{while resolving type '(A.f())'}}
}

func genericParameterInOwnSignature<let N: Int>(_: Vec<N, Int>, _: Vec<(N + 1), Int>) {}
// expected-error@-1 {{global function 'genericParameterInOwnSignature' has self-referential generic requirements}}
// expected-error@-2 {{not supported in a literal expression}}
// expected-error@-3 {{generic value must be an integer literal expression}}
// expected-note@-4 {{while resolving type '(N + 1)'}}
// expected-note@-5 {{while resolving type 'Vec<(N + 1), Int>'}}

func staticMemberInOwnSignature<T>(_: T, _: Vec<(Limits.count), Int>) {}
// expected-error@-1 {{global function 'staticMemberInOwnSignature' has self-referential generic requirements}}
// expected-note@-2 {{while resolving type '(Limits.count)'}}
// expected-note@-3 {{while resolving type 'Vec<(Limits.count), Int>'}}
