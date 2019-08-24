// RUN: %target-typecheck-verify-swift

typealias rgb = Int32 // expected-note {{declared here}}
var rgb : rgb? // expected-error {{invalid redeclaration of 'rgb'}}

// This used to produce a diagnostic about 'rgba' being used in its own
// type, but arguably that is incorrect, since we are referencing a
// different 'rgba'.
struct Color {
    var rgba : rgba? {
        return nil
    }

    typealias rgba = Int32
}

struct Color2 {
    let rgba : rgba?

    struct rgba {}
}

typealias Integer = Int

var i: Integer

struct Hair<Style> {
  typealias Hairdo = Style
  typealias MorningHair = Style?

  func fancy() -> Hairdo {}
  func wakeUp() -> MorningHair {}
}

typealias FunnyHair = Hair<Color>

var f: FunnyHair

// Class inheritance through a typealias.
class BaseClass {}
typealias BaseAlias = BaseClass
class SubClass : BaseAlias {}
let _: BaseClass = SubClass()
let _: BaseAlias = SubClass()

func generic<T: BaseAlias>(_: T) {}
generic(SubClass())
extension BaseAlias {}

class GenericBaseClass<T: AnyObject> {}
typealias GenericBaseAlias = GenericBaseClass
class ConcreteSubClass : GenericBaseAlias<BaseClass> {}
let _: GenericBaseClass<BaseClass> = ConcreteSubClass()
let _: GenericBaseAlias<BaseClass> = ConcreteSubClass()

func generic<T: GenericBaseAlias<BaseClass>>(_: T) {}
generic(ConcreteSubClass())
extension GenericBaseAlias {
  func doSomething(with: T) {}
}

// Protocol adoption through a typealias.
protocol SomeProto {}
typealias SomeProtoAlias = SomeProto
class SomeProtoImpl : SomeProtoAlias {}
let _: SomeProto = SomeProtoImpl()
let _: SomeProtoAlias = SomeProtoImpl()

func generic<T: SomeProtoAlias>(_: T) {}
generic(SomeProtoImpl())
