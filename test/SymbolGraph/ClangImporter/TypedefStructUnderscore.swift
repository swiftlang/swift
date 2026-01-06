// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/TypedefStructUnderscore)
// RUN: split-file %s %t

// RUN: %target-swift-symbolgraph-extract -module-name TypedefStructUnderscore -I %t/TypedefStructUnderscore -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/TypedefStructUnderscore.symbols.json

// _MyStruct.myField, synthesized on MyStruct
// CHECK-DAG: "precise": "c:@S@_MyStruct@FI@myField::SYNTHESIZED::c:TypedefStructUnderscore.h@T@MyStruct"

// MyStruct.myField is a member of MyStruct
// CHECK-DAG: "kind": "memberOf",{{[[:space:]]*}}"source": "c:@S@_MyStruct@FI@myField::SYNTHESIZED::c:TypedefStructUnderscore.h@T@MyStruct",{{[[:space:]]*}}"target": "c:TypedefStructUnderscore.h@T@MyStruct"

// MyStruct automatically conforms to Sendable
// CHECK-DAG: "kind": "conformsTo",{{[[:space:]]*}}"source": "c:TypedefStructUnderscore.h@T@MyStruct",{{[[:space:]]*}}"target": "s:s8SendableP"

//--- TypedefStructUnderscore/module.modulemap
module TypedefStructUnderscore {
  header "TypedefStructUnderscore.h"
}

//--- TypedefStructUnderscore/TypedefStructUnderscore.h
typedef struct _MyStruct {
    int myField;
} MyStruct;
