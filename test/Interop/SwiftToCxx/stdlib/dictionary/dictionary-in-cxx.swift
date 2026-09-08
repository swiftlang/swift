// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name UseDict -cxx-interoperability-mode=default -typecheck -verify -emit-clang-header-path %t/UseDict.h
// RUN: %FileCheck %s < %t/UseDict.h

// RUN: %check-interop-cxx-header-in-clang(%t/UseDict.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

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

// The Dictionary class template is emitted into the generated standard-library
// bindings.
// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: #ifdef __cpp_concepts
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: #endif // __cpp_concepts
// CHECK-NEXT: class SWIFT_SYMBOL("s:SD") Dictionary;

// CHECK: class SWIFT_SYMBOL("s:SD") Dictionary final {

// The thunks for the exposed functions use swift::Dictionary.
// CHECK: swift::Dictionary<swift::String, swift::Int> makeDict(const swift::String& key, swift::Int value)
// CHECK: swift::Dictionary<swift::Int, swift::Array<swift::Int>> makeDictOfArrays()
// CHECK: swift::Optional<swift::Dictionary<swift::Int, swift::Int>> makeOptionalDict()
// CHECK: swift::Int takeDict(const swift::Dictionary<swift::String, swift::Int>& dict)
