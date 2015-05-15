// RUN: %target-swift-frontend -parse -verify -import-cf-types -I %S/Inputs/custom-modules %s

// REQUIRES: objc_interop

import CoreCooling

func assertUnmanaged<T: AnyObject>(t: Unmanaged<T>) {}
func assertManaged<T: AnyObject>(t: T) {}

func test0(fridge: CCRefrigeratorRef) {
  assertManaged(fridge)
}

func test1(power: Unmanaged<CCPowerSupplyRef>) {
  assertUnmanaged(power)
  let fridge = CCRefrigeratorCreate(power) // expected-error {{cannot invoke 'CCRefrigeratorCreate' with an argument list of type '(Unmanaged<CCPowerSupplyRef>)'}} expected-note{{expected an argument list of type '(CCPowerSupply!)'}}
  assertUnmanaged(fridge)
}

func test2() {
  let fridge = CCRefrigeratorCreate(kCCPowerStandard)
  assertUnmanaged(fridge)
}

func test3(fridge: CCRefrigerator) {
  assertManaged(fridge)
}

func test4() {
  // FIXME: this should not require a type annotation
  let power: CCPowerSupply = kCCPowerStandard
  assertManaged(power)
  let fridge = CCRefrigeratorCreate(power)
  assertUnmanaged(fridge)
}

func test5() {
  let power: Unmanaged<CCPowerSupply> = .passUnretained(kCCPowerStandard)
  assertUnmanaged(power)
  _ = CCRefrigeratorCreate(power.takeUnretainedValue())
}

func test6() {
  let fridge = CCRefrigeratorCreate(nil)
  fridge?.release()
}

func test7() {
  let value = CFBottom()
  assertUnmanaged(value)
}

func test8(f: CCRefrigerator) {
  _ = f as CFTypeRef
  _ = f as AnyObject
}

func test9() {
  let fridge = CCRefrigeratorCreateMutable(kCCPowerStandard).takeRetainedValue()
  let constFridge: CCRefrigerator = fridge
  CCRefrigeratorOpen(fridge)
  let item = CCRefrigeratorGet(fridge, 0).takeUnretainedValue()
  CCRefrigeratorInsert(item, fridge) // expected-error {{cannot invoke 'CCRefrigeratorInsert' with an argument list of type '(CCItem, CCMutableRefrigerator)'}} expected-note {{expected an argument list of type '(CCMutableRefrigerator!, CCItem!)'}}
  CCRefrigeratorInsert(constFridge, item) // expected-error {{cannot invoke 'CCRefrigeratorInsert' with an argument list of type '(CCRefrigerator, CCItem)'}} expected-note {{expected an argument list of type '(CCMutableRefrigerator!, CCItem!)'}}
  CCRefrigeratorInsert(fridge, item)
  CCRefrigeratorClose(fridge)
}

func testProperty(k: Kitchen) {
  k.fridge = CCRefrigeratorCreate(kCCPowerStandard).takeRetainedValue()
  CCRefrigeratorOpen(k.fridge)
  CCRefrigeratorClose(k.fridge)
}

func testTollFree0(mduct: MutableDuct) {
  _ = mduct as CCMutableDuct

  let duct = mduct as Duct
  _ = duct as CCDuct
}

func testTollFree1(ccmduct: CCMutableDuct) {
  _ = ccmduct as MutableDuct

  let ccduct: CCDuct = ccmduct
  _ = ccduct as Duct
}

func testChainedAliases(fridge: CCRefrigerator) {
  _ = fridge as CCRefrigeratorRef

  _ = fridge as CCFridge
  _ = fridge as CCFridgeRef
}

func testBannedImported(object: CCOpaqueTypeRef) {
  CCRetain(object) // expected-error {{'CCRetain' is unavailable: Core Foundation objects are automatically memory managed}}
  CCRelease(object) // expected-error {{'CCRelease' is unavailable: Core Foundation objects are automatically memory managed}}
}

func testOutParametersGood() {
  var fridge: CCRefrigerator?
  CCRefrigeratorCreateIndirect(&fridge)

  var power: CCPowerSupply?
  CCRefrigeratorGetPowerSupplyIndirect(fridge!, &power)

  var item: Unmanaged<CCItem>?
  CCRefrigeratorGetItemUnaudited(fridge!, 0, &item)
}

func testOutParametersBad() {
  var fridge: CCRefrigerator?
  CCRefrigeratorCreateIndirect(fridge) // expected-error {{cannot invoke}} expected-note {{'(UnsafeMutablePointer<CCRefrigerator?>)'}}

  var power: CCPowerSupply?
  CCRefrigeratorGetPowerSupplyIndirect(0, power) // expected-error {{cannot invoke}} expected-note {{'(CCRefrigerator!, AutoreleasingUnsafeMutablePointer<CCPowerSupply?>)'}}

  var item: CCItem?
  CCRefrigeratorGetItemUnaudited(0, 0, item) // expected-error {{cannot invoke}} expected-note {{'(CCRefrigerator!, UInt32, UnsafeMutablePointer<Unmanaged<CCItem>?>)'}}
}
