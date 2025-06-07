// RUN: %target-swift-ide-test -print-indexed-symbols -include-locals -source-filename %s | %FileCheck %s

// Helpers
let intValue = 1
let stringValue = ""
let floatValue: Float = 1.0
let doubleValue: Double = 1.0
func calledFunc(value: Int) {}
func tupleReturnType() -> (Int, String) { (1, "") }
typealias TupleTypeAlias = (Int, String)

@propertyWrapper
struct Wrapped<T> {
    let wrappedValue: T
    init(wrappedValue: T) {}
}

// Begin tests

let _: Int = intValue
// CHECK: [[@LINE-1]]:8 | struct/Swift | Int | {{.*}} | Ref | rel: 0
// CHECK: [[@LINE-2]]:14 | variable/Swift | intValue | {{.*}} | Ref,Read | rel: 0

let typedProperty: Int = 1
// CHECK: [[@LINE-1]]:20 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | typedProperty | {{.*}}

let propertyWithExpressionReference = typedProperty
// CHECK: [[@LINE-1]]:39 | variable/Swift | typedProperty | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | propertyWithExpressionReference | {{.*}}
// CHECK: [[@LINE-3]]:39 | function/acc-get/Swift | getter:typedProperty | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | propertyWithExpressionReference | {{.*}}

var propertyWithExplicitAccessors: Int {
    get {
        calledFunc(value: 0)
        // CHECK: [[@LINE-1]]:9 | function/Swift | calledFunc(value:) | {{.*}} | Ref,Call,RelCall,RelCont | rel: 1
        // CHECK-NEXT: RelCall,RelCont | function/acc-get/Swift | getter:propertyWithExplicitAccessors | {{.*}}
        return 0
    }
    set {
        calledFunc(value: 0)
        // CHECK: [[@LINE-1]]:9 | function/Swift | calledFunc(value:) | {{.*}} | Ref,Call,RelCall,RelCont | rel: 1
        // CHECK-NEXT: RelCall,RelCont | function/acc-set/Swift | setter:propertyWithExplicitAccessors | {{.*}}
    }
}

let closureTypedProperty: ((Int) -> Void) = { _ in }
// CHECK: [[@LINE-1]]:29 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | closureTypedProperty | {{.*}}
// CHECK: [[@LINE-3]]:37 | type-alias/Swift | Void | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | closureTypedProperty | {{.*}}

let ((((((parenProperty)))))): ((((((Int)))))) = ((((((intValue))))))
// CHECK: [[@LINE-1]]:38 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | parenProperty | {{.*}}
// CHECK: [[@LINE-3]]:56 | variable/Swift | intValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | parenProperty | {{.*}}
// CHECK: [[@LINE-5]]:56 | function/acc-get/Swift | getter:intValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | parenProperty | {{.*}}

let tupleTypedProperty: (Int, String) = (1, "")
// CHECK: [[@LINE-1]]:26 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleTypedProperty | {{.*}}
// CHECK: [[@LINE-3]]:31 | struct/Swift | String | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleTypedProperty | {{.*}}

let (tupleDestructuredInitElementA, tupleDestructuredInitElementB) = (intValue, stringValue)
// CHECK: [[@LINE-1]]:71 | variable/Swift | intValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleDestructuredInitElementA | {{.*}}
// CHECK: [[@LINE-3]]:71 | function/acc-get/Swift | getter:intValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleDestructuredInitElementA | {{.*}}
// CHECK: [[@LINE-5]]:81 | variable/Swift | stringValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleDestructuredInitElementB | {{.*}}
// CHECK: [[@LINE-7]]:81 | function/acc-get/Swift | getter:stringValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleDestructuredInitElementB | {{.*}}

let (tupleTypedDestructuredInitElementA, tupleTypedDestructuredInitElementB): (Int, String) = (intValue, stringValue)
// CHECK: [[@LINE-1]]:80 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleTypedDestructuredInitElementA | {{.*}}
// CHECK: [[@LINE-3]]:85 | struct/Swift | String | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleTypedDestructuredInitElementB | {{.*}}
// CHECK: [[@LINE-5]]:96 | variable/Swift | intValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleTypedDestructuredInitElementA | {{.*}}
// CHECK: [[@LINE-7]]:96 | function/acc-get/Swift | getter:intValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleTypedDestructuredInitElementA | {{.*}}
// CHECK: [[@LINE-9]]:106 | variable/Swift | stringValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleTypedDestructuredInitElementB | {{.*}}
// CHECK: [[@LINE-11]]:106 | function/acc-get/Swift | getter:stringValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleTypedDestructuredInitElementB | {{.*}}

let (tupleNonDestructuredInitElementA, tupleNonDestructuredInitElementB) = tupleReturnType()
// CHECK: [[@LINE-1]]:76 | function/Swift | tupleReturnType() | {{.*}} | Ref,Call,RelCont | rel: 2
// CHECK-NEXT: RelCont | variable/Swift | tupleNonDestructuredInitElementA | {{.*}}
// CHECK-NEXT: RelCont | variable/Swift | tupleNonDestructuredInitElementB | {{.*}}

let (tupleTypedNonDestructuredInitElementA, tupleTypedNonDestructuredInitElementB): (Int, String) = tupleReturnType()
// CHECK: [[@LINE-1]]:86 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleTypedNonDestructuredInitElementA | {{.*}}
// CHECK: [[@LINE-3]]:91 | struct/Swift | String | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleTypedNonDestructuredInitElementB | {{.*}}
// CHECK: [[@LINE-5]]:101 | function/Swift | tupleReturnType() | {{.*}} | Ref,Call,RelCont | rel: 2
// CHECK-NEXT: RelCont | variable/Swift | tupleTypedNonDestructuredInitElementA | {{.*}}
// CHECK-NEXT: RelCont | variable/Swift | tupleTypedNonDestructuredInitElementB | {{.*}}

let (tupleMultiBindingElementA, tupleMultiBindingElementB): (Int, String) = (1, ""),
    (tupleMultiBindingElementC, tupleMultiBindingElementD) = tupleReturnType(),
    nonTupleMultiBindingProperty = intValue
// CHECK: [[@LINE-3]]:62 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleMultiBindingElementA | {{.*}}
// CHECK: [[@LINE-5]]:67 | struct/Swift | String | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleMultiBindingElementB | {{.*}}
// CHECK: [[@LINE-6]]:62 | function/Swift | tupleReturnType() | {{.*}} | Ref,Call,RelCont | rel: 2
// CHECK-NEXT: RelCont | variable/Swift | tupleMultiBindingElementC | {{.*}}
// CHECK-NEXT: RelCont | variable/Swift | tupleMultiBindingElementD | {{.*}}
// CHECK: [[@LINE-8]]:36 | variable/Swift | intValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | nonTupleMultiBindingProperty | {{.*}}
// CHECK: [[@LINE-10]]:36 | function/acc-get/Swift | getter:intValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | nonTupleMultiBindingProperty | {{.*}}

let (tupleSingleTypedDestructuredInitElementA, tupleSingleTypedDestructuredInitElementB): TupleTypeAlias = (1, "")
// CHECK: [[@LINE-1]]:91 | type-alias/Swift | TupleTypeAlias | {{.*}} | Ref,RelCont | rel: 2
// CHECK-NEXT: RelCont | variable/Swift | tupleSingleTypedDestructuredInitElementA | {{.*}}
// CHECK-NEXT:  RelCont | variable/Swift | tupleSingleTypedDestructuredInitElementB | {{.*}}

let (_, tupleIgnoredSiblingElement): (Int, String) = (intValue, stringValue)
// CHECK: [[@LINE-1]]:39 | struct/Swift | Int | {{.*}} | Ref | rel: 0
// CHECK: [[@LINE-2]]:44 | struct/Swift | String | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleIgnoredSiblingElement | {{.*}}
// CHECK: [[@LINE-4]]:55 | variable/Swift | intValue | {{.*}} | Ref,Read | rel: 0
// CHECK: [[@LINE-5]]:55 | function/acc-get/Swift | getter:intValue | {{.*}} | Ref,Call,Impl | rel: 0
// CHECK: [[@LINE-6]]:65 | variable/Swift | stringValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleIgnoredSiblingElement | {{.*}}
// CHECK: [[@LINE-8]]:65 | function/acc-get/Swift | getter:stringValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT:  RelCont | variable/Swift | tupleIgnoredSiblingElement | {{.*}}

let (tupleNestedElementA, (tupleNestedElementB, (tupleNestedElementC, tupleNestedElementD))): (Int, (String, (Float, Double))) = (intValue, (stringValue, (floatValue, doubleValue)))
// CHECK: [[@LINE-1]]:96 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedElementA | {{.*}}
// CHECK: [[@LINE-3]]:102 | struct/Swift | String | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedElementB | {{.*}}
// CHECK: [[@LINE-5]]:111 | struct/Swift | Float | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedElementC | {{.*}}
// CHECK: [[@LINE-7]]:118 | struct/Swift | Double | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedElementD | {{.*}}
// CHECK: [[@LINE-9]]:131 | variable/Swift | intValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedElementA | {{.*}}
// CHECK: [[@LINE-11]]:131 | function/acc-get/Swift | getter:intValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedElementA | {{.*}}
// CHECK: [[@LINE-13]]:142 | variable/Swift | stringValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedElementB | {{.*}}
// CHECK: [[@LINE-15]]:142 | function/acc-get/Swift | getter:stringValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedElementB | {{.*}}
// CHECK: [[@LINE-17]]:156 | variable/Swift | floatValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedElementC | {{.*}}
// CHECK: [[@LINE-19]]:156 | function/acc-get/Swift | getter:floatValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedElementC | {{.*}}
// CHECK: [[@LINE-21]]:168 | variable/Swift | doubleValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedElementD | {{.*}}
// CHECK: [[@LINE-23]]:168 | function/acc-get/Swift | getter:doubleValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedElementD | {{.*}}

let (tupleNonDestructuredSiblingElementA, (tupleNonDestructuredSiblingElementB)): (Int, (String, (Float, Double))) = (intValue, (stringValue, (floatValue, doubleValue)))
// CHECK: [[@LINE-1]]:84 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNonDestructuredSiblingElementA | {{.*}}
// CHECK: [[@LINE-3]]:90 | struct/Swift | String | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNonDestructuredSiblingElementB | {{.*}}
// CHECK: [[@LINE-5]]:99 | struct/Swift | Float | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNonDestructuredSiblingElementB | {{.*}}
// CHECK: [[@LINE-7]]:106 | struct/Swift | Double | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNonDestructuredSiblingElementB | {{.*}}
// CHECK: [[@LINE-9]]:119 | variable/Swift | intValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNonDestructuredSiblingElementA | {{.*}}
// CHECK: [[@LINE-11]]:119 | function/acc-get/Swift | getter:intValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNonDestructuredSiblingElementA | {{.*}}
// CHECK: [[@LINE-13]]:130 | variable/Swift | stringValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNonDestructuredSiblingElementB | {{.*}}
// CHECK: [[@LINE-15]]:130 | function/acc-get/Swift | getter:stringValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNonDestructuredSiblingElementB | {{.*}}
// CHECK: [[@LINE-17]]:144 | variable/Swift | floatValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNonDestructuredSiblingElementB | {{.*}}
// CHECK: [[@LINE-19]]:144 | function/acc-get/Swift | getter:floatValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNonDestructuredSiblingElementB | {{.*}}
// CHECK: [[@LINE-21]]:156 | variable/Swift | doubleValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNonDestructuredSiblingElementB | {{.*}}
// CHECK: [[@LINE-23]]:156 | function/acc-get/Swift | getter:doubleValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNonDestructuredSiblingElementB | {{.*}}

let (tupleNestedIgnoredSiblingElementA, (tupleNestedIgnoredSiblingElementB, _)): (Int, (String, Float)) = (intValue, (stringValue, floatValue))
// CHECK: [[@LINE-1]]:83 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedIgnoredSiblingElementA | {{.*}}
// CHECK: [[@LINE-3]]:89 | struct/Swift | String | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedIgnoredSiblingElementB | {{.*}}
// CHECK: [[@LINE-5]]:97 | struct/Swift | Float | {{.*}} | Ref | rel: 0
// CHECK: [[@LINE-6]]:108 | variable/Swift | intValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedIgnoredSiblingElementA | {{.*}}
// CHECK: [[@LINE-8]]:108 | function/acc-get/Swift | getter:intValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedIgnoredSiblingElementA | {{.*}}
// CHECK: [[@LINE-10]]:119 | variable/Swift | stringValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedIgnoredSiblingElementB | {{.*}}
// CHECK: [[@LINE-12]]:119 | function/acc-get/Swift | getter:stringValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedIgnoredSiblingElementB | {{.*}}
// CHECK: [[@LINE-14]]:132 | variable/Swift | floatValue | {{.*}} | Ref,Read | rel: 0
// CHECK: [[@LINE-15]]:132 | function/acc-get/Swift | getter:floatValue | {{.*}} | Ref,Call,Impl | rel: 0

let (tupleNestedFuncSiblingElementA, (tupleNestedFuncSiblingElementB, tupleNestedFuncSiblingElementC)): (Double, (Float, (Int, String))) = (doubleValue, (floatValue, tupleReturnType()))
// CHECK: [[@LINE-1]]:106 | struct/Swift | Double | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedFuncSiblingElementA | {{.*}}
// CHECK: [[@LINE-3]]:115 | struct/Swift | Float | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedFuncSiblingElementB | {{.*}}
// CHECK: [[@LINE-5]]:123 | struct/Swift | Int | {{.*}}| Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedFuncSiblingElementC | {{.*}}
// CHECK: [[@LINE-7]]:128 | struct/Swift | String | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedFuncSiblingElementC | {{.*}}
// CHECK: [[@LINE-9]]:141 | variable/Swift | doubleValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedFuncSiblingElementA | {{.*}}
// CHECK: [[@LINE-11]]:141 | function/acc-get/Swift | getter:doubleValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedFuncSiblingElementA | {{.*}}
// CHECK: [[@LINE-13]]:155 | variable/Swift | floatValue | {{.*}} | Ref,Read,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedFuncSiblingElementB | {{.*}}
// CHECK: [[@LINE-15]]:155 | function/acc-get/Swift | getter:floatValue | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedFuncSiblingElementB | {{.*}}
// CHECK: [[@LINE-17]]:167 | function/Swift | tupleReturnType() | {{.*}} | Ref,Call,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | tupleNestedFuncSiblingElementC | {{.*}}

func containingFunc(param: Int) {
    // CHECK: [[@LINE-1]]:6 | function/Swift | containingFunc(param:) | {{.*}} | Def | rel: 0
    // CHECK: [[@LINE-2]]:21 | param/Swift | param | {{.*}} | Def,RelChild | rel: 1
    // CHECK-NEXT: RelChild | function/Swift | containingFunc(param:) | {{.*}}

    let localProperty = param
    // CHECK: [[@LINE-1]]:25 | param/Swift | param | {{.*}} | Ref,Read,RelCont | rel: 1
    // CHECK-NEXT: RelCont | variable(local)/Swift | localProperty | {{.*}}

    calledFunc(value: localProperty)
    // CHECK: [[@LINE-1]]:5 | function/Swift | calledFunc(value:) | {{.*}} | Ref,Call,RelCall,RelCont | rel: 1
    // CHECK-NEXT: RelCall,RelCont | function/Swift | containingFunc(param:) | {{.*}}

    // Ignored declarations do not act as containers.
    let _ = intValue
    // CHECK: [[@LINE-1]]:13 | variable/Swift | intValue | {{.*}} | Ref,Read,RelCont | rel: 1
    // CHECK-NEXT: RelCont | function/Swift | containingFunc(param:) | {{.*}}

    let (_, tupleIgnoredSiblingElementContained): (Int, String) = (intValue, stringValue)
    // CHECK: [[@LINE-1]]:52 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
    // CHECK-NEXT: RelCont | function/Swift | containingFunc(param:) | {{.*}}
    // CHECK: [[@LINE-3]]:57 | struct/Swift | String | {{.*}} | Ref,RelCont | rel: 1
    // CHECK-NEXT: RelCont | variable(local)/Swift | tupleIgnoredSiblingElementContained | {{.*}}
    // CHECK: [[@LINE-5]]:68 | variable/Swift | intValue | {{.*}} | Ref,Read,RelCont | rel: 1
    // CHECK-NEXT: RelCont | function/Swift | containingFunc(param:) | {{.*}}
    // CHECK: [[@LINE-7]]:68 | function/acc-get/Swift | getter:intValue | {{.*}} | Ref,Call,Impl,RelCall,RelCont | rel: 1
    // CHECK-NEXT: RelCall,RelCont | function/Swift | containingFunc(param:) | {{.*}}
    // CHECK: [[@LINE-9]]:78 | variable/Swift | stringValue | {{.*}} | Ref,Read,RelCont | rel: 1
    // CHECK-NEXT: RelCont | variable(local)/Swift | tupleIgnoredSiblingElementContained | {{.*}}
    // CHECK: [[@LINE-11]]:78 | function/acc-get/Swift | getter:stringValue | {{.*}} | Ref,Call,Impl,RelCall,RelCont | rel: 2
    // CHECK-NEXT: RelCont | variable(local)/Swift | tupleIgnoredSiblingElementContained | {{.*}}

    let (_, tupleIgnoredSiblingElementContained): (Int, String) = (
      { let x = intValue; return x }(),
      { let y = stringValue; return y }()
    )
    // CHECK:      [[@LINE-3]]:13 | variable(local)/Swift | x | {{.*}} | Def,RelChild | rel: 1
    // CHECK-NEXT: RelChild | function/Swift | containingFunc(param:)

    // CHECK:      [[@LINE-6]]:17 | variable/Swift | intValue | {{.*}} | Ref,Read,RelCont | rel: 1
    // CHECK-NEXT: RelCont | variable(local)/Swift | x

    // Here the reference to intValue is contained by 'x'.
    // CHECK:      [[@LINE-10]]:17 | function/acc-get/Swift | getter:intValue | {{.*}} | Ref,Call,Impl,RelCall,RelCont | rel: 2
    // CHECK-NEXT: RelCont | variable(local)/Swift | x
    // CHECK-NEXT: RelCall | function/Swift | containingFunc(param:)

    // But here the container for the reference to 'x' is the parent function.
    // CHECK:      [[@LINE-15]]:34 | variable(local)/Swift | x | {{.*}} | Ref,Read,RelCont | rel: 1
    // CHECK-NEXT: RelCont | function/Swift | containingFunc(param:)

    // CHECK:      [[@LINE-18]]:34 | function/acc-get(local)/Swift | getter:x | {{.*}} | Ref,Call,Impl,RelCall,RelCont | rel: 1
    // CHECK-NEXT: RelCall,RelCont | function/Swift | containingFunc(param:)

    // CHECK:      [[@LINE-20]]:13 | variable(local)/Swift | y | {{.*}} | Def,RelChild | rel: 1
    // CHECK-NEXT: RelChild | function/Swift | containingFunc(param:)

    // CHECK:      [[@LINE-23]]:17 | variable/Swift | stringValue | {{.*}} | Ref,Read,RelCont | rel: 1
    // CHECK-NEXT: RelCont | variable(local)/Swift | y

    // Here the reference to stringValue is contained by 'y'.
    // CHECK:      [[@LINE-27]]:17 | function/acc-get/Swift | getter:stringValue | {{.*}} | Ref,Call,Impl,RelCall,RelCont | rel: 2
    // CHECK-NEXT: RelCont | variable(local)/Swift | y
    // CHECK-NEXT: RelCall | function/Swift | containingFunc(param:)

    // But here the container for the reference to 'y' is the parent binding.
    // CHECK:      [[@LINE-32]]:37 | variable(local)/Swift | y | {{.*}} | Ref,Read,RelCont | rel: 1
    // CHECK-NEXT: RelCont | variable(local)/Swift | tupleIgnoredSiblingElementContained

    // CHECK:      [[@LINE-35]]:37 | function/acc-get(local)/Swift | getter:y | {{.*}} | Ref,Call,Impl,RelCall,RelCont | rel: 2
    // CHECK-NEXT: RelCont | variable(local)/Swift | tupleIgnoredSiblingElementContained
    // CHECK-NEXT: RelCall | function/Swift | containingFunc(param:)
}

func functionWithReturnType() -> Int { 0 }
// CHECK: [[@LINE-1]]:34 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | function/Swift | functionWithReturnType() | {{.*}}

func functionWithParameter(a: Int) {}
// CHECK: [[@LINE-1]]:31 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | function/Swift | functionWithParameter(a:) | {{.*}}

func functionWithGenericConstraint<T: Equatable>(type: T) {}
// CHECK: [[@LINE-1]]:39 | protocol/Swift | Equatable | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | function/Swift | functionWithGenericConstraint(type:) | {{.*}}

func functionWithGenericClause<T>(type: T) where T: Equatable {}
// CHECK: [[@LINE-1]]:53 | protocol/Swift | Equatable | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | function/Swift | functionWithGenericClause(type:) | {{.*}}

struct SomeStruct {
    static let staticProperty: Int = 1
    // CHECK: [[@LINE-1]]:32 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
    // CHECK-NEXT: RelCont | static-property/Swift | staticProperty | {{.*}}

    lazy var lazyProperty: Int = { 1 }()
    // CHECK: [[@LINE-1]]:14 | instance-method/acc-get/Swift | getter:lazyProperty | s:14swift_ide_test10SomeStructV12lazyPropertySivg | Def,Impl,RelChild,RelAcc | rel: 1
    // CHECK-NEXT: RelChild,RelAcc | instance-property/Swift | lazyProperty | s:14swift_ide_test10SomeStructV12lazyPropertySivp
    // CHECK: [[@LINE-3]]:14 | instance-method/acc-set/Swift | setter:lazyProperty | s:14swift_ide_test10SomeStructV12lazyPropertySivs | Def,Impl,RelChild,RelAcc | rel: 1
    // CHECK-NEXT: RelChild,RelAcc | instance-property/Swift | lazyProperty | s:14swift_ide_test10SomeStructV12lazyPropertySivp
    // CHECK: [[@LINE-5]]:28 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
    // CHECK-NEXT: RelCont | instance-property/Swift | lazyProperty | {{.*}}

    @Wrapped
    var wrappedProperty: Int = 1
    // CHECK: [[@LINE-2]]:6 | struct/Swift | Wrapped | {{.*}} | Ref,RelCont | rel: 1
    // CHECK-NEXT: RelCont | instance-property/Swift | wrappedProperty | {{.*}}
    // CHECK: [[@LINE-4]]:6 | constructor/Swift | init(wrappedValue:) | {{.*}} | Ref,Call,Impl,RelCont | rel: 1
    // CHECK-NEXT: RelCont | instance-property/Swift | wrappedProperty | {{.*}}
    // CHECK: [[@LINE-5]]:9 | instance-property/Swift | wrappedProperty | {{.*}} | Def,RelChild | rel: 1
    // CHECK: [[@LINE-6]]:26 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
    // CHECK-NEXT: RelCont | instance-property/Swift | wrappedProperty | {{.*}}

    init(a: Int) {}
    // CHECK: [[@LINE-1]]:13 | struct/Swift | Int | {{.*}} | Ref,RelCont | rel: 1
    // CHECK-NEXT: RelCont | constructor/Swift | init(a:) | {{.*}}
}

let voidProperty: () = ()
// CHECK: [[@LINE-1]]:5 | variable/Swift | voidProperty | {{.*}} | Def | rel: 0

let voidAliasedProperty: Void = ()
// CHECK: [[@LINE-1]]:5 | variable/Swift | voidAliasedProperty | {{.*}} | Def | rel: 0
// CHECK: [[@LINE-2]]:26 | type-alias/Swift | Void | {{.*}} | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | variable/Swift | voidAliasedProperty | {{.*}}

var computedVoidProperty: () { () }
// CHECK: [[@LINE-1]]:5 | variable/Swift | computedVoidProperty | {{.*}} | Def | rel: 0

func voidFunc() -> () { () }
// CHECK: [[@LINE-1]]:6 | function/Swift | voidFunc() | {{.*}} | Def | rel: 0
