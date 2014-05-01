// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -verify -import-cf-types -module-cache-path %t/clang-module-cache -I %S/Inputs/custom-modules -target x86_64-apple-darwin13 %s

import CoreCooling

func assertUnmanaged<T:AnyObject>(t: Unmanaged<T>) {}

func test0(fridge: CCRefrigeratorRef) {
  assertUnmanaged(fridge)
}

func test1(power: CCPowerSupplyRef) {
  assertUnmanaged(power)
  let fridge = CCRefrigeratorCreate(power)
  assertUnmanaged(fridge)
}

func test2() {
  let fridge = CCRefrigeratorCreate(kCCPowerStandard)
  assertUnmanaged(fridge)
}

