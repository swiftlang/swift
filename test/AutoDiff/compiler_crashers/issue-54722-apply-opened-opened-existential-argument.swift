// RUN: %target-swift-frontend -disable-availability-checking -emit-sil -verify %s

// https://github.com/apple/swift/issues/54722
// Differentiation transform crash for `apply` with opened existential arguments

import _Differentiation

public protocol TensorView {
  associatedtype Element
}
public protocol DifferentiableTensorView: TensorView & Differentiable where Self == TangentVector {}
// expected-warning@-1 {{protocol 'DifferentiableTensorView' should be declared to refine 'AdditiveArithmetic' due to a same-type constraint on 'Self'}}

public protocol PlatformAPI {
  func abs<T>(_ x: T) -> T where T: DifferentiableTensorView, T.Element: Numeric
}
public class CpuService: PlatformAPI {
  public func abs<T>(_ x: T) -> T where T: DifferentiableTensorView, T.Element: Numeric { x }
}

public final class Platform {
  public static var service: PlatformAPI = CpuService()
}

@differentiable(reverse where T: DifferentiableTensorView)
public func abs<T: DifferentiableTensorView>(_ x: T) -> T where T.Element: Numeric {
  Platform.service.abs(x)
  // expected-error@-1 {{expression is not differentiable}}
  // expected-note@-2 {{member is not differentiable because the corresponding protocol requirement is not '@differentiable'}}
}

// swift: swift/lib/AST/ASTContext.cpp:3307: swift::SILFunctionType::SILFunctionType(swift::GenericSignature, swift::SILFunctionType::ExtInfo, swift::SILCoroutineKind, swift::ParameterConvention, ArrayRef<swift::SILParameterInfo>, ArrayRef<swift::SIL
// YieldInfo>, ArrayRef<swift::SILResultInfo>, Optional<swift::SILResultInfo>, swift::SubstitutionMap, swift::SubstitutionMap, const swift::ASTContext &, swift::RecursiveTypeProperties, swift::ProtocolConformanceRef): Assertion `!WitnessMethodConformance.isInvalid() && "witness_method SIL function without a conformance"' failed.
// Stack dump:
// 0.      Program arguments: build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift -frontend -c -primary-file swift/test/AutoDiff/compiler_crashers/tf1181-apply-opened-opened-existential-argument.swift -target x86_64-unknown-linux-gnu -disable-objc-interop -color-diagnostics -module-name main -o /tmp/tf1181-apply-opened-opened-existential-argument-3a917d.o
// 1.      Swift version 5.3-dev (LLVM f66b332548, Swift dee6c0b09f)
// 2.      While evaluating request ExecuteSILPipelineRequest(Run pipelines { Guaranteed Passes } on SIL for main.main)
// 3.      While running pass #105 SILModuleTransform "Differentiation".
// 4.      While processing // differentiability witness for abs<A>(_:)
// sil_differentiability_witness [serialized] [reverse] [parameters 0] [results 0] <T where T : DifferentiableTensorView, T.Element : Numeric> @$s4main3absyxxAA24DifferentiableTensorViewRzSj7ElementAA0dE0PRpzlF : $@convention(thin) <T where T : DifferentiableTensorView, T.Element : Numeric> (@in_guaranteed T) -> @out T {
// }
//  on SIL function "@$s4main3absyxxAA24DifferentiableTensorViewRzSj7ElementAA0dE0PRpzlF".
//  for 'abs(_:)' (at swift/test/AutoDiff/compiler_crashers/tf1181-apply-opened-opened-existential-argument.swift:27:8)
//  #0 0x00000000051bd514 PrintStackTraceSignalHandler(void*) (build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x51bd514)
//  #1 0x00000000051bb10e llvm::sys::RunSignalHandlers() (build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x51bb10e)
//  #2 0x00000000051bd7ec SignalHandler(int) (build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x51bd7ec)
//  #3 0x00007f160b5e2890 __restore_rt (/lib/x86_64-linux-gnu/libpthread.so.0+0x12890)
//  #4 0x00007f1609e84e97 raise (/lib/x86_64-linux-gnu/libc.so.6+0x3ee97)
//  #5 0x00007f1609e86801 abort (/lib/x86_64-linux-gnu/libc.so.6+0x40801)
//  #6 0x00007f1609e7639a (/lib/x86_64-linux-gnu/libc.so.6+0x3039a)
//  #7 0x00007f1609e76412 (/lib/x86_64-linux-gnu/libc.so.6+0x30412)
//  #8 0x00000000017998be swift::SILFunctionType::SILFunctionType(swift::GenericSignature, swift::SILFunctionType::ExtInfo, swift::SILCoroutineKind, swift::ParameterConvention, llvm::ArrayRef<swift::SILParameterInfo>, llvm::ArrayRef<swift::SILYieldInfo>, llvm::ArrayRef<swift::SILResultInfo>, llvm::Optional<swift::SILResultInfo>, swift::SubstitutionMap, swift::SubstitutionMap, swift::ASTContext const&, swift::RecursiveTypeProperties, swift::ProtocolConformanceRef) (build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x17998be)
//  #9 0x000000000179a806 swift::SILFunctionType::get(swift::GenericSignature, swift::SILFunctionType::ExtInfo, swift::SILCoroutineKind, swift::ParameterConvention, llvm::ArrayRef<swift::SILParameterInfo>, llvm::ArrayRef<swift::SILYieldInfo>, llvm::ArrayRef<swift::SILResultInfo>, llvm::Optional<swift::SILResultInfo>, swift::SubstitutionMap, swift::SubstitutionMap, swift::ASTContext const&, swift::ProtocolConformanceRef) (build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x179a806)
// #10 0x00000000010bf7e8 (anonymous namespace)::SILTypeSubstituter::substSILFunctionType(swift::CanTypeWrapper<swift::SILFunctionType>, bool) (build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x10bf7e8)
// #11 0x00000000010be86c swift::SILType::subst(swift::Lowering::TypeConverter&, llvm::function_ref<swift::Type (swift::SubstitutableType*)>, llvm::function_ref<swift::ProtocolConformanceRef (swift::CanType, swift::Type, swift::ProtocolDecl*)>, swift::CanGenericSignature, bool) const (build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x10be86c)
// #12 0x00000000010ae3fe swift::GenericEnvironment::mapTypeIntoContext(swift::SILModule&, swift::SILType) const (build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0x10ae3fe)
// #13 0x0000000000f1a1e1 swift::autodiff::LinearMapInfo::addLinearMapToStruct(swift::autodiff::ADContext&, swift::ApplyInst*) (build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0xf1a1e1)
// #14 0x0000000000f17925 swift::autodiff::LinearMapInfo::generateDifferentiationDataStructures(swift::autodiff::ADContext&, swift::SILFunction*) (build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0xf17925)
// #15 0x0000000000f2615f swift::autodiff::VJPEmitter::VJPEmitter(swift::autodiff::ADContext&, swift::SILFunction*, swift::SILDifferentiabilityWitness*, swift::SILFunction*, swift::autodiff::DifferentiationInvoker) (build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0xf2615f)
// #16 0x0000000000e7efff (anonymous namespace)::DifferentiationTransformer::canonicalizeDifferentiabilityWitness(swift::SILFunction*, swift::SILDifferentiabilityWitness*, swift::autodiff::DifferentiationInvoker, swift::SerializedKind_t) (build/Ninja-ReleaseAssert/swift-linux-x86_64/bin/swift+0xe7efff)
