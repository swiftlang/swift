// RUN: %empty-directory(%t)

// REQUIRES: swift_feature_CoroutineFunctions

// Tests that we prefer the normalized target triple name for a .swiftmodule,
// but fall back to the legacy architecture name if necessary.

// RUN: mkdir %t/TargetLibrary.swiftmodule
// RUN: %target-swift-frontend -emit-module -o %t/TargetLibrary.swiftmodule/%module-target-triple.swiftmodule %S/Inputs/def_func.swift -module-name TargetLibrary -enable-experimental-feature CoroutineFunctions
// RUN: touch %t/TargetLibrary.swiftmodule/%target-swiftmodule-name

import TargetLibrary

// RUN: mkdir %t/ArchLibrary.swiftmodule
// RUN: %target-swift-frontend -emit-module -o %t/ArchLibrary.swiftmodule/%target-swiftmodule-name %S/Inputs/def_func.swift -module-name ArchLibrary -enable-experimental-feature CoroutineFunctions

import ArchLibrary

// RUN: mkdir -p %t/TargetModule.framework/Modules/TargetModule.swiftmodule
// RUN: %target-swift-frontend -emit-module -o %t/TargetModule.framework/Modules/TargetModule.swiftmodule/%module-target-triple.swiftmodule %S/Inputs/def_func.swift -module-name TargetModule -enable-experimental-feature CoroutineFunctions
// RUN: touch %t/TargetModule.framework/Modules/TargetModule.swiftmodule/%target-swiftmodule-name

import TargetModule

// RUN: mkdir -p %t/ArchModule.framework/Modules/ArchModule.swiftmodule
// RUN: %target-swift-frontend -emit-module -o %t/ArchModule.framework/Modules/ArchModule.swiftmodule/%target-swiftmodule-name %S/Inputs/def_func.swift -module-name ArchModule -enable-experimental-feature CoroutineFunctions

import ArchModule

// RUN: %target-swift-frontend %s -typecheck -I %t -F %t
