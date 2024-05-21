// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/Core.swift \
// RUN: -module-name=Core -package-name Pkg \
// RUN: -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -enable-library-evolution -O -wmo \
// RUN: -emit-ir -o %t/Core.ir \
// RUN: -emit-tbd -emit-tbd-path %t/libCore.tbd \
// RUN: -Xfrontend -tbd-install_name=libCore.dylib -Xfrontend -validate-tbd-against-ir=all

// RUN: %FileCheck %s --check-prefix=CHECK-IR < %t/Core.ir
// RUN: %FileCheck %s --check-prefix=CHECK-TBD < %t/libCore.tbd

//--- Core.swift

final public class Pub {}

package class Foo {
  package var myFoo: Pub?
}

final package class Bar {
  package var myBar: Pub?
}

// key path getter for Core.Foo.myFoo
// CHECK-IR-DAG: define linkonce_odr hidden swiftcc void @"$s4Core3FooC02myB0AA3PubCSgvpACTK"

// key path setter for Core.Foo.myFoo
// CHECK-IR-DAG: define linkonce_odr hidden swiftcc void @"$s4Core3FooC02myB0AA3PubCSgvpACTk"

// variable initialization expression of Core.Foo.myFoo
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc {{i32|i64}} @"$s4Core3FooC02myB0AA3PubCSgvpfi"() #0 {

// Core.Foo.myFoo.getter
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc {{i32|i64}} @"$s4Core3FooC02myB0AA3PubCSgvg"(ptr swiftself %0)

// merged Core.Foo.myFoo.getter
// CHECK-IR-DAG: define internal swiftcc {{i32|i64}} @"$s4Core3FooC02myB0AA3PubCSgvgTm"(ptr swiftself %0)

// Core.Foo.myFoo.setter
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc void @"$s4Core3FooC02myB0AA3PubCSgvs"({{i32|i64}} %0, ptr swiftself %1) #1 {

// merged Core.Foo.myFoo.setter
// CHECK-IR-DAG: define internal swiftcc void @"$s4Core3FooC02myB0AA3PubCSgvsTm"({{i32|i64}} %0, ptr swiftself %1)

// Core.Foo.myFoo.modify
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc { ptr, ptr } @"$s4Core3FooC02myB0AA3PubCSgvM"

// Core.Foo.myFoo.modify
// CHECK-IR-DAG: define internal swiftcc void @"$s4Core3FooC02myB0AA3PubCSgvM.resume.0"

// type metadata accessor for Core.Foo
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc %swift.metadata_response @"$s4Core3FooCMa"

// method lookup function for Core.Foo
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc ptr @"$s4Core3FooCMu"(ptr %0, ptr %1)

// dispatch thunk of Core.Foo.myFoo.getter
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc {{i32|i64}} @"$s4Core3FooC02myB0AA3PubCSgvgTj"(ptr swiftself %0)

// dispatch thunk of Core.Foo.myFoo.setter
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc void @"$s4Core3FooC02myB0AA3PubCSgvsTj"({{i32|i64}} %0, ptr swiftself %1)

// dispatch thunk of Core.Foo.myFoo.modify
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc { ptr, ptr } @"$s4Core3FooC02myB0AA3PubCSgvMTj"

// Core.Foo.deinit
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc ptr @"$s4Core3FooCfd"(ptr readonly returned swiftself %0)

// Core.Foo.__deallocating_deinit
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc void @"$s4Core3FooCfD"(ptr swiftself %0)


// Core.Bar.myBar
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc {{i32|i64}} @"$s4Core3BarC02myB0AA3PubCSgvpfi"()
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc {{i32|i64}} @"$s4Core3BarC02myB0AA3PubCSgvg"(ptr swiftself %0)
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc void @"$s4Core3BarC02myB0AA3PubCSgvs"({{i32|i64}} %0, ptr swiftself %1)
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc { ptr, ptr } @"$s4Core3BarC02myB0AA3PubCSgvM"
// CHECK-IR-DAG: define internal swiftcc void @"$s4Core3BarC02myB0AA3PubCSgvM.resume.0"

// Core.Bar
// type metadata accessor for Core.Bar
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc %swift.metadata_response @"$s4Core3BarCMa"({{i32|i64}} %0)

// method lookup function for Core.Bar
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc ptr @"$s4Core3BarCMu"(ptr %0, ptr %1)

// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc ptr @"$s4Core3BarCfd"(ptr readonly returned swiftself %0)
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc void @"$s4Core3BarCfD"(ptr swiftself %0)

// Core.Pub
// type metadata accessor for Core.Pub
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc %swift.metadata_response @"$s4Core3PubCMa"({{i32|i64}} %0)

// method lookup function for Core.Pub
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc ptr @"$s4Core3PubCMu"(ptr %0, ptr %1)

// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc ptr @"$s4Core3PubCfd"(ptr readnone returned swiftself %0)
// CHECK-IR-DAG: define {{(dllexport |protected )?}}swiftcc void @"$s4Core3PubCfD"(ptr swiftself %0)


// property descriptor for Core.Foo.myFoo
// CHECK-TBD-DAG: s4Core3FooC02myB0AA3PubCSgvpMV
// method descriptor for Core.Foo.myFoo.getter
// CHECK-TBD-DAG: s4Core3FooC02myB0AA3PubCSgvgTq
// method descriptor for Core.Foo.myFoo.setter
// CHECK-TBD-DAG: s4Core3FooC02myB0AA3PubCSgvsTq
// method descriptor for Core.Foo.myFoo.modify
// CHECK-TBD-DAG: s4Core3FooC02myB0AA3PubCSgvMTq
// dispatch thunk of Core.Foo.myFoo.getter
// CHECK-TBD-DAG: s4Core3FooC02myB0AA3PubCSgvgTj
// dispatch thunk of Core.Foo.myFoo.setter
// CHECK-TBD-DAG: s4Core3FooC02myB0AA3PubCSgvsTj
// dispatch thunk of Core.Foo.myFoo.modify
// CHECK-TBD-DAG: s4Core3FooC02myB0AA3PubCSgvMTj
// type metadata accessor for Core.Foo
// CHECK-TBD-DAG: s4Core3FooCMa
// method lookup function for Core.Foo
// CHECK-TBD-DAG: s4Core3FooCMu
// nominal type descriptor for Core.Foo
// CHECK-TBD-DAG: s4Core3FooCMn
// class metadata base offset for Core.Foo
// CHECK-TBD-DAG: s4Core3FooCMo

// CHECK-TBD-DAG: s4Core3FooC02myB0AA3PubCSgvpfi
// CHECK-TBD-DAG: s4Core3FooC02myB0AA3PubCSgvg
// CHECK-TBD-DAG: s4Core3FooC02myB0AA3PubCSgvs
// CHECK-TBD-DAG: s4Core3FooCfd
// CHECK-TBD-DAG: s4Core3FooCfD

// CHECK-TBD-DAG: s4Core3BarC02myB0AA3PubCSgvpMV
// CHECK-TBD-DAG: s4Core3BarC02myB0AA3PubCSgvpfi
// CHECK-TBD-DAG: s4Core3BarC02myB0AA3PubCSgvg
// CHECK-TBD-DAG: s4Core3BarC02myB0AA3PubCSgvs
// CHECK-TBD-DAG: s4Core3BarC02myB0AA3PubCSgvM
// CHECK-TBD-DAG: s4Core3BarCMa
// CHECK-TBD-DAG: s4Core3BarCMu
// CHECK-TBD-DAG: s4Core3BarCMn
// CHECK-TBD-DAG: s4Core3BarCMo
// CHECK-TBD-DAG: s4Core3BarCfd
// CHECK-TBD-DAG: s4Core3BarCfD

// CHECK-TBD-DAG: s4Core3PubCMa
// CHECK-TBD-DAG: s4Core3PubCMu
// CHECK-TBD-DAG: s4Core3PubCMn
// CHECK-TBD-DAG: s4Core3PubCMo
// CHECK-TBD-DAG: s4Core3PubCfd
// CHECK-TBD-DAG: s4Core3PubCfD
