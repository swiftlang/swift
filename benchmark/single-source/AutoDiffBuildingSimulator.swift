//===--- AutoDiffBuildingSimulator.swift ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils
import _Differentiation

public let benchmarks = [
  BenchmarkInfo(
    name: "AutoDiffBuildingSimulator.Forward",
    runFunction: run_AutoDiffBuildingSimulatorForward,
    tags: [.validation, .api, .algorithm]),
  BenchmarkInfo(
    name: "AutoDiffBuildingSimulator.Reverse",
    runFunction: run_AutoDiffBuildingSimulatorReverse,
    tags: [.validation, .api, .algorithm]),
  
]

// Simulation parameters
let trials = 100
let timesteps = 20
let dTime: Float = 0.1
let printGradToCompare = false

// Definitions
let π = Float.pi

struct SimParams: Differentiable {
    var tube: TubeType = .init()
    var slab: SlabType = .init()
    var quanta: QuantaType = .init()
    var tank: TankType = .init()
    var startingTemp: Float
}

struct TubeType: Differentiable {
    var tubeSpacing: Float = 0.50292 // meters
    var diameter: Float = 0.019 // m  (3/4")
    var thickness: Float = 0.001588 // m (1/16")
    var resistivity: Float = 2.43 // (K/W)m
}

struct SlabType: Differentiable {
    var temp: Float = 21.1111111 // °C
    var area: Float = 100.0 // m^2
    var Cp: Float = 0.2
    var density: Float = 2242.58 // kg/m^3
    var thickness: Float = 0.101 // m
}

struct QuantaType: Differentiable {
    var power: Float = 0.0 // Watt
    var temp: Float = 60.0 // °C
    var flow: Float = 0.0006309 // m^3/sec
    var density: Float = 1000.0 // kg/m^3
    var Cp: Float = 4180.0 // ws/(kg • K)
}

struct TankType: Differentiable {
    var temp: Float = 70.0
    var volume: Float = 0.0757082
    var Cp: Float = 4180.000
    var density: Float = 1000.000
    var mass: Float = 75.708
}

// Computations

@differentiable(reverse)
func computeResistance(floor: SlabType, tube: TubeType, quanta _: QuantaType) -> Float {
    let geometry_coeff: Float = 10.0
    // let f_coff = 0.3333333

    let tubingSurfaceArea = (floor.area / tube.tubeSpacing) * π * tube.diameter
    let resistance_abs = tube.resistivity * tube.thickness / tubingSurfaceArea

    let resistance_corrected = resistance_abs * geometry_coeff // * (quanta.flow * f_coff)

    return resistance_corrected
}

struct QuantaAndPower: Differentiable {
    var quanta: QuantaType
    var power: Float
}


extension Differentiable {
    /// Applies the given closure to the derivative of `self`.
    ///
    /// Returns `self` like an identity function. When the return value is used in
    /// a context where it is differentiated with respect to, applies the given
    /// closure to the derivative of the return value.
    @inlinable
    @differentiable(reverse, wrt: self)
    func withDerivative(_: @escaping (inout TangentVector) -> Void) -> Self {
        return self
    }

    @inlinable
    @derivative(of: withDerivative)
    func _vjpWithDerivative(
        _ body: @escaping (inout TangentVector) -> Void
    ) -> (value: Self, pullback: (TangentVector) -> TangentVector) {
        return (self, { grad in
            var grad = grad
            body(&grad)
            return grad
        })
    }
}

@differentiable(reverse)
func computeLoadPower(floor: SlabType, tube: TubeType, quanta: QuantaType) -> QuantaAndPower {
    let resistance_abs = computeResistance(floor: floor, tube: tube, quanta: quanta)

    let conductance: Float = 1 / resistance_abs
    let dTemp = floor.temp - quanta.temp
    let power = dTemp * conductance

    var updatedQuanta = quanta
    updatedQuanta.power = power
    let loadPower = -power

    return QuantaAndPower(quanta: updatedQuanta, power: loadPower)
}

@differentiable(reverse)
func updateQuanta(quanta: QuantaType) -> QuantaType {
    let workingVolume = (quanta.flow * dTime)
    let workingMass = (workingVolume * quanta.density)
    let workingEnergy = quanta.power * dTime
    let TempRise = workingEnergy / quanta.Cp / workingMass
    var updatedQuanta = quanta
    updatedQuanta.temp = quanta.temp + TempRise

    updatedQuanta.power = 0
    return updatedQuanta
}

@differentiable(reverse)
func updateBuildingModel(power: Float, floor: SlabType) -> SlabType {
    var updatedFloor = floor

    let floorVolume = floor.area * floor.thickness
    let floorMass = floorVolume * floor.density

    updatedFloor.temp = floor.temp + ((power * dTime) / floor.Cp / floorMass)
    return updatedFloor
}

struct TankAndQuanta: Differentiable {
    var tank: TankType
    var quanta: QuantaType
}

@differentiable(reverse)
func updateSourceTank(store: TankType, quanta: QuantaType) -> TankAndQuanta {
    var updatedStore = store
    var updatedQuanta = quanta

    let massPerTime = quanta.flow * quanta.density
    let dTemp = store.temp - quanta.temp
    let power = dTemp * massPerTime * quanta.Cp

    updatedQuanta.power = power

    let tankMass = store.volume * store.density
    let TempRise = (power * dTime) / store.Cp / tankMass
    updatedStore.temp = store.temp + TempRise

    return TankAndQuanta(tank: updatedStore, quanta: updatedQuanta)
}

var simParams = SimParams(startingTemp: 33.3)

@differentiable(reverse)
@inlinable public func absDifferentiable(_ value: Float) -> Float {
    if value < 0 {
        return -value
    }
    return value
}

func lossCalc(pred: Float, gt: Float) -> Float {
    let diff = pred - gt
    return absDifferentiable(diff)
}

// Simulations

@differentiable(reverse)
func simulate(simParams: SimParams) -> Float {
    let pexTube = simParams.tube
    var slab = simParams.slab
    var tank = simParams.tank
    var quanta = simParams.quanta

    slab.temp = simParams.startingTemp
    for _ in 0 ..< timesteps {
        let tankAndQuanta = updateSourceTank(store: tank, quanta: quanta)
        tank = tankAndQuanta.tank
        quanta = tankAndQuanta.quanta

        quanta = updateQuanta(quanta: quanta)

        let quantaAndPower = computeLoadPower(floor: slab, tube: pexTube, quanta: quanta)
        quanta = quantaAndPower.quanta
        let powerToBuilding = quantaAndPower.power
        quanta = updateQuanta(quanta: quanta)

        slab = updateBuildingModel(power: powerToBuilding, floor: slab)
    }
    return slab.temp
}

var blackHole: Any?
@inline(never)
func dontLetTheCompilerOptimizeThisAway<T>(_ x: T) {
    blackHole = x
}

@differentiable(reverse)
func fullPipe(simParams: SimParams) -> Float {
    let pred = simulate(simParams: simParams)
    let loss = lossCalc(pred: pred, gt: 27.344767)
    return loss
}

@inline(never)
public func run_AutoDiffBuildingSimulatorForward(n: Int) {
  for _ in 0 ..< n {
    let forwardOnly = fullPipe(simParams: simParams)
    dontLetTheCompilerOptimizeThisAway(forwardOnly)
  }
}

@inline(never)
public func run_AutoDiffBuildingSimulatorReverse(n: Int) {
  for _ in 0 ..< n {
    let grad = gradient(at: simParams, of: fullPipe)
    dontLetTheCompilerOptimizeThisAway(grad)
  }
}
