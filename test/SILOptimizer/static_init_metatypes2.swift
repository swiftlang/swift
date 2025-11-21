// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -parse-as-library -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -O
// XXX: %target-swift-frontend -parse-as-library -I %t %t/Main.swift -O -emit-sil | %FileCheck %s --check-prefix SIL
// RUN: %target-swift-frontend -parse-as-library -I %t %t/Main.swift -O -emit-ir | %FileCheck %s --check-prefix IR

// BEGIN MyModule.swift

public class MyClass {}
public struct MyStruct {}

// BEGIN Main.swift

import MyModule

public let classMetatype: Any.Type = MyClass.self
public let classMetatypeArray: [Any.Type] = [classMetatype]

public let structMetatype: Any.Type = MyStruct.self
public let structMetatypeArray: [Any.Type] = [structMetatype]

// SIL:       sil_global [let] @$s4Main13classMetatypeypXpvp : $@thick any Any.Type
// SIL-EMPTY: 
// SIL:       sil_global [let] @$s4Main18classMetatypeArraySayypXpGvp : $Array<any Any.Type>
// SIL-EMPTY: 
// SIL:       sil_global [let] @$s4Main14structMetatypeypXpvp : $@thick any Any.Type = {
// SIL-NEXT:    %0 = metatype $@thick MyStruct.Type
// SIL-NEXT:   %initval = init_existential_metatype %0, $@thick any Any.Type
// SIL-NEXT:  }
// SIL:       sil_global [let] @$s4Main19structMetatypeArraySayypXpGvp : $Array<any Any.Type>
// SIL-EMPTY:

// IR: @"$s4Main13classMetatypeypXpvp" = {{.*}}global { ptr } zeroinitializer
// IR: @"$s4Main18classMetatypeArraySayypXpGvp" = {{.*}}global %TSa zeroinitializer
// IR: @"$s8MyModule0A6StructVN" = {{.*}}global %swift.type
// IR: @"$s4Main14structMetatypeypXpvp" = {{.*}}constant ptr @"$s8MyModule0A6StructVN"
// IR: @"$s4Main19structMetatypeArraySayypXpGvp" = {{.*}}global %TSa zeroinitializer
