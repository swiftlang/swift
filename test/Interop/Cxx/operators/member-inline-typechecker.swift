// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop

import MemberInline

var lhs = LoadableIntWrapper(value: 42)
let rhs = LoadableIntWrapper(value: 23)

let resultPlus = lhs - rhs
let resultCall0 = lhs()
let resultCall1 = lhs(1)
let resultCall2 = lhs(1, 2)

var boolWrapper = LoadableBoolWrapper(value: true)
let notBoolResult = !boolWrapper

var addressOnly = AddressOnlyIntWrapper(42)

let addressOnlyResultCall0 = addressOnly()
let addressOnlyResultCall1 = addressOnly(1)
let addressOnlyResultCall2 = addressOnly(1, 2)

var readWriteIntArray = ReadWriteIntArray()
readWriteIntArray[2] = 321
let readWriteValue = readWriteIntArray[2]

var readOnlyIntArray = ReadOnlyIntArray(3)
let readOnlyValue = readOnlyIntArray[1]

var writeOnlyIntArray = WriteOnlyIntArray()
writeOnlyIntArray[2] = 654
let writeOnlyValue = writeOnlyIntArray[2]

var diffTypesArray = DifferentTypesArray()
let diffTypesResultInt: Int32 = diffTypesArray[0]
let diffTypesResultDouble: Double = diffTypesArray[0.5]

var nonTrivialIntArrayByVal = NonTrivialIntArrayByVal(3)
let nonTrivialValueByVal = nonTrivialIntArrayByVal[1]

var diffTypesArrayByVal = DifferentTypesArrayByVal()
let diffTypesResultIntByVal: Int32 = diffTypesArrayByVal[0]
let diffTypesResultDoubleByVal: Double = diffTypesArrayByVal[0.5]

let postIncrement = HasPostIncrementOperator()
postIncrement.successor() // expected-error {{value of type 'HasPostIncrementOperator' has no member 'successor'}}

let anotherReturnType = HasPreIncrementOperatorWithAnotherReturnType()
anotherReturnType.successor() // expected-error {{value of type 'HasPreIncrementOperatorWithAnotherReturnType' has no member 'successor'}}
