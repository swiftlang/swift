// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-ns-options-as-option-set-type -parse %s -verify
// -- Check that we can successfully round-trip.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-ns-options-as-option-set-type -D IRGEN -emit-ir %s >/dev/null

// REQUIRES: objc_interop

import Foundation
import user_objc

var withMince: NSRuncingOptions = .EnableMince
var withQuince: NSRuncingOptions = .EnableQuince

// When there is a single enum constant, compare it against the type name to
// derive the namespaced name.
var singleValue: NSSingleOptions = .Value

// Check OptionSetType conformance.
func useOptionSet<T: OptionSetType>(x: T) {}
useOptionSet(singleValue)
useOptionSet(withMince)
useOptionSet(withQuince)

var minceFromMask: NSRuncingOptions = NSRuncingOptions(rawValue: 0)
var minceValue: UInt = minceFromMask.rawValue

/* TODO: OptionSet operations
var minceAndQuince: NSRuncingOptions = .EnableMince.intersect(.EnableQuince)
var minceOrQuince: NSRuncingOptions = .EnableMince.union(.EnableQuince)
var noMince: NSRuncingOptions = NSRuncingOptions.EnableMince.inverse
minceOrQuince.intersectInPlace(noMince)
minceOrQuince.unionInPlace(minceAndQuince)

var nothing: NSRuncingOptions = NSRuncingOptions()
var nothing2: NSRuncingOptions = nil
let nothing3: NSRuncingOptions = .allZeros
 */
