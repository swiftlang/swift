// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift_driver -emit-module -emit-objc-header -o %t/Base.swiftmodule %S/Inputs/resolve-cross-language/Base.swift
// RUN: cp %S/Inputs/resolve-cross-language/Base-module.map %t/module.map
// RUN: %swift %clang-importer-sdk -module-cache-path %t -parse -I %t -F %S/Inputs/resolve-cross-language %s -verify

import Base
import BaseUser

// Sanity check.
useBaseClass(getBaseClass())
useBaseClassObjC(getBaseClassObjC())

// Check type resolution.
useBaseClass(getBaseClassObjC())
useBaseClassObjC(getBaseClass())
