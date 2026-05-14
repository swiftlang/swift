// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractMemberFunctions.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s
// RUN: cat %t/ExtractMemberFunctions.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto {}

struct MyStruct {
    func myMethod(arg: String) -> String { 
        "" 
    }
    static func myStaticFunc() -> MyStruct {
        MyStruct()
    }
    func me() -> MyStruct {
        self
    }
}

class MyClass {
    func myMethod(arg: String) -> Int {
        5
    }
}

struct Statics: MyProto {
    var prop1 = MyStruct().myMethod(arg: "struct arg")
    var prop2 = MyClass().myMethod(arg: "class arg")
    var prop3 = MyStruct().myMethod(arg: "second struct").lowercased()
    var prop4 = MyStruct.myStaticFunc().me()
}

// CHECK:       "label": "prop1",
// CHECK-NEXT:  "type": "Swift.String",
// CHECK:       "valueKind": "MemberFunctionCall",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "baseValue": {
// CHECK-NEXT:      "valueKind": "InitCall",
// CHECK-NEXT:      "value": {
// CHECK-NEXT:        "type": "ExtractMemberFunctions.MyStruct",
// CHECK-NEXT:        "arguments": []
// CHECK-NEXT:      }
// CHECK-NEXT:    },
// CHECK-NEXT:    "calls": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "memberLabel": "myMethod",
// CHECK-NEXT:        "arguments": [
// CHECK-NEXT:          {
// CHECK-NEXT:            "label": "arg",
// CHECK-NEXT:            "type": "Swift.String",
// CHECK-NEXT:            "valueKind": "RawLiteral",
// CHECK-NEXT:            "value": "struct arg"
// CHECK-NEXT:          }
// CHECK-NEXT:        ]
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
// CHECK-NEXT:  }

// CHECK:       "label": "prop2",
// CHECK-NEXT:  "type": "Swift.Int",
// CHECK:       "valueKind": "MemberFunctionCall",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "baseValue": {
// CHECK-NEXT:      "valueKind": "InitCall",
// CHECK-NEXT:      "value": {
// CHECK-NEXT:        "type": "ExtractMemberFunctions.MyClass",
// CHECK-NEXT:        "arguments": []
// CHECK-NEXT:      }
// CHECK-NEXT:    },
// CHECK-NEXT:    "calls": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "memberLabel": "myMethod",
// CHECK-NEXT:        "arguments": [
// CHECK-NEXT:          {
// CHECK-NEXT:            "label": "arg",
// CHECK-NEXT:            "type": "Swift.String",
// CHECK-NEXT:            "valueKind": "RawLiteral",
// CHECK-NEXT:            "value": "class arg"
// CHECK-NEXT:          }
// CHECK-NEXT:        ]
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
// CHECK-NEXT:  }

// CHECK:       "label": "prop3",
// CHECK-NEXT:  "type": "Swift.String",
// CHECK:       "valueKind": "MemberFunctionCall",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "baseValue": {
// CHECK-NEXT:      "valueKind": "InitCall",
// CHECK-NEXT:      "value": {
// CHECK-NEXT:        "type": "ExtractMemberFunctions.MyStruct",
// CHECK-NEXT:        "arguments": []
// CHECK-NEXT:      }
// CHECK-NEXT:    },
// CHECK-NEXT:    "calls": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "memberLabel": "myMethod",
// CHECK-NEXT:        "arguments": [
// CHECK-NEXT:          {
// CHECK-NEXT:            "label": "arg",
// CHECK-NEXT:            "type": "Swift.String",
// CHECK-NEXT:            "valueKind": "RawLiteral",
// CHECK-NEXT:            "value": "second struct"
// CHECK-NEXT:          }
// CHECK-NEXT:        ]
// CHECK-NEXT:      },
// CHECK-NEXT:      {
// CHECK-NEXT:        "memberLabel": "lowercased",
// CHECK-NEXT:        "arguments": []
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
// CHECK-NEXT:  }

// CHECK:       "label": "prop4",
// CHECK-NEXT:  "type": "ExtractMemberFunctions.MyStruct",
// CHECK:       "valueKind": "MemberFunctionCall",
// CHECK-NEXT:  "value": {
// CHECK-NEXT:    "baseValue": {
// CHECK-NEXT:      "valueKind": "StaticFunctionCall",
// CHECK-NEXT:      "value": {
// CHECK-NEXT:        "type": "ExtractMemberFunctions.MyStruct",
// CHECK-NEXT:        "memberLabel": "myStaticFunc",
// CHECK-NEXT:        "arguments": []
// CHECK-NEXT:      }
// CHECK-NEXT:    },
// CHECK-NEXT:    "calls": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "memberLabel": "me",
// CHECK-NEXT:        "arguments": []
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
// CHECK-NEXT:  }
