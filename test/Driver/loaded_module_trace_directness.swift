// Check "isImportedDirectly" in trace for complex dependency graphs.
// See also: rdar://64993153.

// Unsupported on Windows because we are using #import in ObjC, which leads
// to compiler errors on Windows.
// UNSUPPORTED: OS=windows-msvc

// Dependency graph:

// * CoreShip - ObjC module with overlay
// * CoreFuel - ObjC module with overlay
// * CoreMission - Swift module
// * LaunchKit - ObjC module with overlay, has #import <CoreFuel.h> but not import CoreFuel
// * FlightKit - Swift module, has import CoreShip and @_exported import CoreMission
// * ShipUI - Swift module, has import FlightKit and import LaunchKit

// RUN: %empty-directory(%t)

// 1. Create CoreShip module
// RUN: %target-swift-frontend %s -DCoreShip -module-name CoreShip -emit-module -o %t/CoreShip.swiftmodule -I %S/Inputs/imported_modules/ComplexModuleGraph

#if CoreShip
@_exported import CoreShip
#endif


// 2. Create CoreFuel module
// RUN: %target-swift-frontend %s -DCoreFuel -module-name CoreFuel -emit-module -o %t/CoreFuel.swiftmodule -I %S/Inputs/imported_modules/ComplexModuleGraph

#if CoreFuel
@_exported import CoreFuel
public func totalEnergyInMJ(hydrogenInKg: Float, methaneInKg: Float) -> Float {
  return hydrogenInKg * hydrogenEnergyDensityInMJPerKg
       + methaneInKg * methaneEnergyDensityInMJPerKg
}
#endif


// 3. Create CoreMission module
// RUN: %target-swift-frontend %s -DCoreMission -module-name CoreMission -emit-module -o %t/CoreMission.swiftmodule

#if CoreMission
public typealias Planet = String
public typealias Mission = (origin: Planet, destination: Planet)
#endif


// 4. Compile LaunchKit and check that its trace has:
// - direct dependency on CoreFuel
//
// RUN: %target-swift-frontend %s -DLaunchKit -module-name LaunchKit -emit-module -o %t/LaunchKit.swiftmodule -I %S/Inputs/imported_modules/ComplexModuleGraph -I %t -emit-loaded-module-trace
// RUN: %FileCheck %s --check-prefix=LAUNCHKIT < %t/LaunchKit.trace.json

#if LaunchKit
// CoreFuel is not imported explicitly, but it is automatically @_exported via ObjC #import
@_exported import LaunchKit

// Both ObjC and Swift parts of CoreFuel are directly accessible!
let _ = CoreFuel.hydrogenEnergyDensityInMJPerKg
let _ = CoreFuel.totalEnergyInMJ

// LAUNCHKIT: {"name":"CoreFuel","path":"{{[^"]*}}CoreFuel.swiftmodule","isImportedDirectly":true,
#endif


// 5. Compile FlightKit and check that trace has:
// - direct dependency on CoreShip
//
// RUN: %target-swift-frontend %s -DFlightKit -module-name FlightKit -emit-module -o %t/FlightKit.swiftmodule -I %S/Inputs/imported_modules/ComplexModuleGraph -I %t -emit-loaded-module-trace
// RUN: %FileCheck %s --check-prefix=FLIGHTKIT < %t/FlightKit.trace.json

#if FlightKit
import CoreShip
@_exported import CoreMission

// FLIGHTKIT: "swiftmodulesDetailedInfo":[
// FLIGHTKIT-DAG: {"name":"CoreShip","path":"{{[^"]*}}CoreShip.swiftmodule","isImportedDirectly":true,
// FLIGHTKIT-DAG: {"name":"CoreMission","path":"{{[^"]*}}CoreMission.swiftmodule","isImportedDirectly":true,
// FLIGHTKIT: ]
#endif

// 6. Compile ShipUI and check that trace has:
// - direct dependency on FlightKit and LaunchKit (due to imports)
// - direct dependency on CoreFuel (via LaunchKit)
// - indirect dependency on CoreShip (via FlightKit)
// - direct dependency on CoreMission (via FlightKit)
//
// RUN: %target-swift-frontend %s -DShipUI -module-name ShipUI -emit-module -o %t/ShipUI.swiftmodule -I %S/Inputs/imported_modules/ComplexModuleGraph -I %t -emit-loaded-module-trace
// RUN: %FileCheck %s --check-prefix=SHIPUI < %t/ShipUI.trace.json

#if ShipUI
import FlightKit
import LaunchKit

// Both ObjC and Swift parts of CoreFuel are accessible via LaunchKit
let _ = LaunchKit.hydrogenEnergyDensityInMJPerKg
let _ = LaunchKit.totalEnergyInMJ

// CoreMission's types are accessible via FlightKit
let _cassini: FlightKit.Mission = (origin: "Earth", destination: "Saturn")

// SHIPUI: "swiftmodulesDetailedInfo":[
// SHIPUI-DAG: {"name":"FlightKit","path":"{{[^"]*}}FlightKit.swiftmodule","isImportedDirectly":true,
// SHIPUI-DAG: {"name":"LaunchKit","path":"{{[^"]*}}LaunchKit.swiftmodule","isImportedDirectly":true,
// SHIPUI-DAG: {"name":"CoreFuel","path":"{{[^"]*}}CoreFuel.swiftmodule","isImportedDirectly":true,
// SHIPUI-DAG: {"name":"CoreShip","path":"{{[^"]*}}CoreShip.swiftmodule","isImportedDirectly":false,
// SHIPUI-DAG: {"name":"CoreMission","path":"{{[^"]*}}CoreMission.swiftmodule","isImportedDirectly":true,
// SHIPUI: ]
#endif
