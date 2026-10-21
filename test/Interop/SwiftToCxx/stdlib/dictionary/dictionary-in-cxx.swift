// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/use-dict.swift -module-name UseDict -cxx-interoperability-mode=default -enable-experimental-feature GenerateBindingsForHashableRequirementsInCXX -typecheck -verify -emit-clang-header-path %t/UseDict.h
// RUN: %FileCheck %s --implicit-check-not=lE9hashValue < %t/UseDict.h

// RUN: %check-interop-cxx-header-in-clang(%t/UseDict.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// Without the feature, Dictionary is not exposed.
// RUN: %target-swift-frontend %t/use-dict.swift -module-name UseDictNoFeature -cxx-interoperability-mode=default -typecheck -verify -emit-clang-header-path %t/UseDictNoFeature.h
// RUN: %FileCheck %s --check-prefix=NO-FEATURE < %t/UseDictNoFeature.h

// The standard library bindings are printed by the first generated header that
// is included. A header generated with the feature reports an error if they
// were printed without Dictionary.
// RUN: not %target-interop-build-clangxx -std=c++20 -fsyntax-only -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY %t/include-without-feature-first.cpp -I %t 2>&1 | %FileCheck %s --check-prefix=MIXED
// RUN: %target-interop-build-clangxx -std=c++20 -fsyntax-only -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY %t/include-with-feature-first.cpp -I %t

// REQUIRES: swift_feature_GenerateBindingsForHashableRequirementsInCXX

// The Dictionary class template is emitted into the generated standard-library
// bindings.
// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: #ifdef __cpp_concepts
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: #endif // __cpp_concepts
// CHECK-NEXT: class SWIFT_SYMBOL("s:SD") Dictionary;

// Members that need an extra Hashable conformance, like `hashValue` from
// `extension Array: Hashable where Element: Hashable`, are not exposed on
// Array, Optional or Dictionary (see the implicit-check-not above).
// CHECK: class SWIFT_SYMBOL("s:SD") Dictionary final {
// CHECK: static SWIFT_INLINE_THUNK Dictionary<T_0_0, T_0_1> init() noexcept SWIFT_SYMBOL("s:S2Dyxq_Gycfc");
// CHECK: static SWIFT_INLINE_THUNK Dictionary<T_0_0, T_0_1> init(swift::Int minimumCapacity) noexcept SWIFT_SYMBOL("s:SD15minimumCapacitySDyxq_GSi_tcfc");
// CHECK: SWIFT_INLINE_THUNK swift::Optional<T_0_1> updateValueForKey(const T_0_1& value, const T_0_0& key) noexcept SWIFT_SYMBOL("s:SD11updateValue_6forKeyq_Sgq_n_xtF");
// CHECK: SWIFT_INLINE_THUNK swift::Optional<T_0_1> removeValueForKey(const T_0_0& key) noexcept SWIFT_SYMBOL("s:SD11removeValue6forKeyq_Sgx_tF");
// CHECK: SWIFT_INLINE_THUNK void removeAllKeepingCapacity(bool keepCapacity) noexcept SWIFT_SYMBOL("s:SD9removeAll15keepingCapacityySb_tF");
// CHECK: SWIFT_INLINE_THUNK swift::Int getCount() const noexcept SWIFT_SYMBOL("s:SD5countSivp");
// CHECK: SWIFT_INLINE_THUNK bool isEmpty() const noexcept SWIFT_SYMBOL("s:SD7isEmptySbvp");
// CHECK: SWIFT_INLINE_THUNK String getDescription() const noexcept SWIFT_SYMBOL("s:SD11descriptionSSvp");
// CHECK: SWIFT_INLINE_THUNK String getDebugDescription() const noexcept SWIFT_SYMBOL("s:SD16debugDescriptionSSvp");
// CHECK: SWIFT_INLINE_THUNK swift::Int getCapacity() const noexcept SWIFT_SYMBOL("s:SD8capacitySivp");
// CHECK: SWIFT_INLINE_THUNK void reserveCapacity(swift::Int minimumCapacity) noexcept SWIFT_SYMBOL("s:SD15reserveCapacityyySiF");
// CHECK: SWIFT_INLINE_THUNK swift::Optional<T_0_1> operator [](const T_0_0& key) const noexcept SWIFT_SYMBOL("s:SDyq_Sgxcig");

// The thunks for the exposed functions use swift::Dictionary.
// CHECK: swift::Dictionary<swift::String, swift::Int> makeDict(const swift::String& key, swift::Int value)
// CHECK: swift::Dictionary<swift::Int, swift::Array<swift::Int>> makeDictOfArrays()
// CHECK: swift::Optional<swift::Dictionary<swift::Int, swift::Int>> makeOptionalDict()
// CHECK: swift::Int takeDict(const swift::Dictionary<swift::String, swift::Int>& dict)

// NO-FEATURE-NOT: class SWIFT_SYMBOL("s:SD") Dictionary
// NO-FEATURE: // Unavailable in C++: Swift global function 'makeDict(_:_:)'.
// NO-FEATURE-NOT: class SWIFT_SYMBOL("s:SD") Dictionary

// MIXED: error: "the Swift standard library bindings were printed by a header generated without GenerateBindingsForHashableRequirementsInCXX; enable the feature for all Swift modules whose headers are included, or include this header first"

//--- use-dict.swift
@_expose(Cxx)
public func makeDict(_ key: String, _ value: Int) -> [String: Int] {
    return [key: value]
}

@_expose(Cxx)
public func takeDict(_ dict: [String: Int]) -> Int {
    return dict.count
}

@_expose(Cxx)
public func makeOptionalDict() -> [Int: Int]? {
    return nil
}

@_expose(Cxx)
public func makeDictOfArrays() -> [Int: [Int]] {
    return [1: [2, 3]]
}

//--- include-without-feature-first.cpp
#include "UseDictNoFeature.h"
#include "UseDict.h"

//--- include-with-feature-first.cpp
#include "UseDict.h"
#include "UseDictNoFeature.h"
