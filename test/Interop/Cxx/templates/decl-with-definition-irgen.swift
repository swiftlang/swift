// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import DeclWithDefinition

public func getWrappedMagicNumber() -> CInt {
  let magicNumber = MagicNumber()
  var wrappedMagicNumber = PartiallyDefinedWrappedMagicNumber(t: magicNumber)
  return wrappedMagicNumber.callGetInt()
}

// CHECK: define {{(protected |dllexport )?}}swiftcc i32 @"$s4main21getWrappedMagicNumbers5Int32VyF"()
// CHECK: %wrappedMagicNumber = alloca %TSo12MagicWrapperV, align 1
// CHECK: %wrappedMagicNumber.t = getelementptr inbounds %TSo12MagicWrapperV, %TSo12MagicWrapperV* %wrappedMagicNumber, i32 0, i32 0
// CHECK: [[MAGIC_WRAPPER:%.*]] = bitcast %TSo12MagicWrapperV* %wrappedMagicNumber to %struct.MagicWrapper*
// CHECK: call i32 @{{_ZNK12MagicWrapperI11MagicNumberE10callGetIntEv|"\?callGetInt@\?\$MagicWrapper@UMagicNumber@@@@QEBAHXZ"}}(%struct.MagicWrapper* [[MAGIC_WRAPPER]])

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

