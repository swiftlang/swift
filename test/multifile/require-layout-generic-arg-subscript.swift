// RUN: %target-swift-frontend -module-name test -emit-ir -verify -primary-file %s %S/Inputs/require-layout-generic-class.swift | %FileCheck --check-prefix=FILE1 %s
// RUN: %target-swift-frontend -module-name test -emit-ir -verify %s -primary-file %S/Inputs/require-layout-generic-class.swift | %FileCheck --check-prefix=FILE2 %s

// REQUIRES: CPU=x86_64

// The offset of the typemetadata in the class typemetadata must match.

// FILE1: define hidden swiftcc i64 @_T04test12AccessorTestCSiAA3SubCyxGcluig(%T4test3SubC*, %T4test12AccessorTestC* swiftself)
// FILE1:   [[T1:%.*]] = bitcast %T4test3SubC* %0 to %swift.type**
// FILE1:   [[TYPEMETADATA:%.*]] = load %swift.type*, %swift.type** [[T1]]
// FILE1:   [[T2:%.*]] = bitcast %swift.type* [[TYPEMETADATA]] to %swift.type**
// This offset must match the offset below.
// FILE1:   [[T_IN_CLASSMETADATA:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[T2]], i64 16
// FILE1:   [[T:%.*]] = load %swift.type*, %swift.type** [[T_IN_CLASSMETADATA]]
// FILE1:   call %swift.type* @swift_getMetatypeMetadata(%swift.type* [[T]])
public class AccessorTest {
  subscript<T>(_ a: Sub<T>) -> Int {
    get {
      print(T.self)
      return 1
    }
  }
}

// FILE2-LABEL: define private %swift.type* @create_generic_metadata_Sub(%swift.type_pattern*, i8**)
// FILE2:   [[T_ADDR:%.*]] = bitcast i8** %1 to %swift.type**
// FILE2:   [[T:%.*]] = load %swift.type*, %swift.type** %2
// FILE2:   [[CLASSMETADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata
// FILE2:   [[ADDR:%.*]] = bitcast %swift.type* [[CLASSMETADATA]] to i8**
// This offset must match the offset above.
// FILE2:   [[T_IN_CLASSMETADATA:%.*]] = getelementptr inbounds i8*, i8** [[ADDR]], i32 16
// FILE2:   [[T2:%.*]] = bitcast %swift.type* [[T]] to i8*
// FILE2:   store i8* [[T2]], i8** [[T_IN_CLASSMETADATA]]
