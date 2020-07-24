// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import CanonicalTypes

public func testCanonicalTypes() -> Bool {
  // multiple typedeffed types with the same canonical type are the same type
  // from the typechecking perspective.
  let magicNumber = MagicNumber()
  var wrappedMagicNumberA = WrappedMagicNumberA(t: magicNumber)

  var wrappedMagicNumberB: WrappedMagicNumberA =
    WrappedMagicNumberB(t: magicNumber)
  return wrappedMagicNumberA.callGetInt() == wrappedMagicNumberB.callGetInt()
}

// CHECK_NOT: __CxxTemplateInst
// CHECK: define {{(protected |dllexport )?}}swiftcc i1 @"$s4main18testCanonicalTypesSbyF"()
// CHECK: %wrappedMagicNumberA = alloca %TSo12MagicWrapperV, align 1
// CHECK: %wrappedMagicNumberB = alloca %TSo12MagicWrapperV, align 1
// CHECK: %wrappedMagicNumberA.t = getelementptr inbounds %TSo12MagicWrapperV, %TSo12MagicWrapperV* %wrappedMagicNumberA, i32 0, i32 0
// CHECK: %wrappedMagicNumberB.t = getelementptr inbounds %TSo12MagicWrapperV, %TSo12MagicWrapperV* %wrappedMagicNumberB, i32 0, i32 0
// CHECK: [[MAGIC_WRAPPER_A:%.*]] = bitcast %TSo12MagicWrapperV* %wrappedMagicNumberA to %struct.MagicWrapper*
// CHECK: call i32 @{{_ZNK12MagicWrapperI11MagicNumberE10callGetIntEv|"\?callGetInt@\?\$MagicWrapper@UMagicNumber@@@@QEBAHXZ"}}(%struct.MagicWrapper* [[MAGIC_WRAPPER_A]])
// CHECK: [[MAGIC_WRAPPER_B:%.*]] = bitcast %TSo12MagicWrapperV* %wrappedMagicNumberB to %struct.MagicWrapper*
// CHECK: call i32 @{{_ZNK12MagicWrapperI11MagicNumberE10callGetIntEv|"\?callGetInt@\?\$MagicWrapper@UMagicNumber@@@@QEBAHXZ"}}(%struct.MagicWrapper* [[MAGIC_WRAPPER_B]])

// CHECK: define weak_odr{{( dso_local)?}} i32 @{{_ZNK12MagicWrapperI11MagicNumberE10callGetIntEv|"\?callGetInt@\?\$MagicWrapper@UMagicNumber@@@@QEBAHXZ"}}(%struct.MagicWrapper* %this)
// CHECK: %this.addr = alloca %struct.MagicWrapper*, align {{4|8}}
// CHECK: store %struct.MagicWrapper* %this, %struct.MagicWrapper** %this.addr, align {{4|8}}
// CHECK: %this1 = load %struct.MagicWrapper*, %struct.MagicWrapper** %this.addr, align {{4|8}}
// CHECK: %t = getelementptr inbounds %struct.MagicWrapper, %struct.MagicWrapper* %this1, i32 0, i32 0
// CHECK: %call = call i32 @{{_ZNK11MagicNumber6getIntEv|"\?getInt@MagicNumber@@QEBAHXZ"}}(%struct.MagicNumber* %t)
// CHECK: %add = add nsw i32 %call, 5
// CHECK: ret i32 %add

// CHECK: define linkonce_odr{{( dso_local)?}} i32 @{{_ZNK11MagicNumber6getIntEv|"\?getInt@MagicNumber@@QEBAHXZ"}}(%struct.MagicNumber* %this)
// CHECK: %this.addr = alloca %struct.MagicNumber*, align {{4|8}}
// CHECK: store %struct.MagicNumber* %this, %struct.MagicNumber** %this.addr, align {{4|8}}
// CHECK: %this1 = load %struct.MagicNumber*, %struct.MagicNumber** %this.addr, align {{4|8}}
// CHECK: ret i32 24

