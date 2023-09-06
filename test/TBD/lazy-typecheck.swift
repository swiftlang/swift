// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -target arm64-apple-macosx10.13 -swift-version 5 %S/../Inputs/lazy_typecheck.swift -module-name lazy_typecheck -emit-module -emit-module-path /dev/null -emit-tbd-path %t/lazy_typecheck.tbd -enable-library-evolution -parse-as-library -package-name Package -experimental-lazy-typecheck -experimental-skip-all-function-bodies -experimental-serialize-external-decls-only
// RUN: %llvm-readtapi %t/lazy_typecheck.tbd %t/expected.tbd

// REQUIRES: OS=macosx

//--- expected.tbd
--- !tapi-tbd
tbd-version:     4
targets:         [ arm64-macos ]
flags:           [ not_app_extension_safe ]
install-name:    ''
current-version: 0
compatibility-version: 0
swift-abi-version: 7
exports:
  - targets:         [ arm64-macos ]
    symbols:         [ '_$s14lazy_typecheck023PublicClassConformingToC5ProtoC3reqSiyFTj',
                       '_$s14lazy_typecheck023PublicClassConformingToC5ProtoC3reqSiyFTq',
                       '_$s14lazy_typecheck023PublicClassConformingToC5ProtoCAA0cG0AAMc',
                       '_$s14lazy_typecheck023PublicClassConformingToC5ProtoCAA0cG0AAWP',
                       '_$s14lazy_typecheck023PublicClassConformingToC5ProtoCACycfC',
                       '_$s14lazy_typecheck023PublicClassConformingToC5ProtoCACycfCTj',
                       '_$s14lazy_typecheck023PublicClassConformingToC5ProtoCACycfCTq',
                       '_$s14lazy_typecheck023PublicClassConformingToC5ProtoCACycfc',
                       '_$s14lazy_typecheck023PublicClassConformingToC5ProtoCMa',
                       '_$s14lazy_typecheck023PublicClassConformingToC5ProtoCMm',
                       '_$s14lazy_typecheck023PublicClassConformingToC5ProtoCMn',
                       '_$s14lazy_typecheck023PublicClassConformingToC5ProtoCMo',
                       '_$s14lazy_typecheck023PublicClassConformingToC5ProtoCMu',
                       '_$s14lazy_typecheck023PublicClassConformingToC5ProtoCN',
                       '_$s14lazy_typecheck023PublicClassConformingToC5ProtoCfD',
                       '_$s14lazy_typecheck023PublicClassConformingToC5ProtoCfd',
                       '_$s14lazy_typecheck024PublicStructConformingToC5ProtoV3reqSiyF',
                       '_$s14lazy_typecheck024PublicStructConformingToC5ProtoVAA0cG0AAMc',
                       '_$s14lazy_typecheck024PublicStructConformingToC5ProtoVAA0cG0AAWP',
                       '_$s14lazy_typecheck024PublicStructConformingToC5ProtoVACycfC',
                       '_$s14lazy_typecheck024PublicStructConformingToC5ProtoVMa',
                       '_$s14lazy_typecheck024PublicStructConformingToC5ProtoVMn',
                       '_$s14lazy_typecheck024PublicStructConformingToC5ProtoVN',
                       '_$s14lazy_typecheck034PublicClassInheritingConformanceToC5ProtoCACycfC',
                       '_$s14lazy_typecheck034PublicClassInheritingConformanceToC5ProtoCACycfc',
                       '_$s14lazy_typecheck034PublicClassInheritingConformanceToC5ProtoCMa',
                       '_$s14lazy_typecheck034PublicClassInheritingConformanceToC5ProtoCMm',
                       '_$s14lazy_typecheck034PublicClassInheritingConformanceToC5ProtoCMn',
                       '_$s14lazy_typecheck034PublicClassInheritingConformanceToC5ProtoCMo',
                       '_$s14lazy_typecheck034PublicClassInheritingConformanceToC5ProtoCN',
                       '_$s14lazy_typecheck034PublicClassInheritingConformanceToC5ProtoCfD',
                       '_$s14lazy_typecheck034PublicClassInheritingConformanceToC5ProtoCfd',
                       '_$s14lazy_typecheck034PublicStructIndirectlyConformingToC5ProtoV3reqSiyF',
                       '_$s14lazy_typecheck034PublicStructIndirectlyConformingToC5ProtoVAA0cH0AAMc',
                       '_$s14lazy_typecheck034PublicStructIndirectlyConformingToC5ProtoVAA0cH0AAWP',
                       '_$s14lazy_typecheck034PublicStructIndirectlyConformingToC5ProtoVACycfC',
                       '_$s14lazy_typecheck034PublicStructIndirectlyConformingToC5ProtoVMa',
                       '_$s14lazy_typecheck034PublicStructIndirectlyConformingToC5ProtoVMn',
                       '_$s14lazy_typecheck034PublicStructIndirectlyConformingToC5ProtoVN',
                       '_$s14lazy_typecheck10publicFuncSiyF', '_$s14lazy_typecheck11PublicClassC06publicD6MethodyyFZTj',
                       '_$s14lazy_typecheck11PublicClassC06publicD6MethodyyFZTq',
                       '_$s14lazy_typecheck11PublicClassC12publicMethodSiyFTj', '_$s14lazy_typecheck11PublicClassC12publicMethodSiyFTq',
                       '_$s14lazy_typecheck11PublicClassC1xACSi_tcfC', '_$s14lazy_typecheck11PublicClassC1xACSi_tcfCTj',
                       '_$s14lazy_typecheck11PublicClassC1xACSi_tcfCTq', '_$s14lazy_typecheck11PublicClassC1xACSi_tcfc',
                       '_$s14lazy_typecheck11PublicClassCMa', '_$s14lazy_typecheck11PublicClassCMm',
                       '_$s14lazy_typecheck11PublicClassCMn', '_$s14lazy_typecheck11PublicClassCMo',
                       '_$s14lazy_typecheck11PublicClassCMu', '_$s14lazy_typecheck11PublicClassCN',
                       '_$s14lazy_typecheck11PublicClassCfD', '_$s14lazy_typecheck11PublicClassCfd',
                       '_$s14lazy_typecheck11PublicProtoMp', '_$s14lazy_typecheck11PublicProtoP3reqSiyFTj',
                       '_$s14lazy_typecheck11PublicProtoP3reqSiyFTq', '_$s14lazy_typecheck11PublicProtoTL',
                       '_$s14lazy_typecheck11packageFuncSiyF', '_$s14lazy_typecheck12PublicStructV12publicMethodSiyF',
                       '_$s14lazy_typecheck12PublicStructV18publicStaticMethodyyFZ',
                       '_$s14lazy_typecheck12PublicStructV1xACSi_tcfC', '_$s14lazy_typecheck12PublicStructVMa',
                       '_$s14lazy_typecheck12PublicStructVMn', '_$s14lazy_typecheck12PublicStructVN',
                       '_$s14lazy_typecheck13inlinableFuncSiyF', '_$s14lazy_typecheck18PublicDerivedClassC1xACSi_tcfC',
                       '_$s14lazy_typecheck18PublicDerivedClassC1xACSi_tcfc', '_$s14lazy_typecheck18PublicDerivedClassCMa',
                       '_$s14lazy_typecheck18PublicDerivedClassCMm', '_$s14lazy_typecheck18PublicDerivedClassCMn',
                       '_$s14lazy_typecheck18PublicDerivedClassCMo', '_$s14lazy_typecheck18PublicDerivedClassCN',
                       '_$s14lazy_typecheck18PublicDerivedClassCfD', '_$s14lazy_typecheck18PublicDerivedClassCfd',
                       '_$s14lazy_typecheck19PublicRethrowsProtoMp', '_$s14lazy_typecheck19PublicRethrowsProtoP3reqSiyKFTj',
                       '_$s14lazy_typecheck19PublicRethrowsProtoP3reqSiyKFTq', '_$s14lazy_typecheck19PublicRethrowsProtoTL',
                       '_$s14lazy_typecheck24publicFuncWithDefaultArgyS2iF', '_$s14lazy_typecheck30publicFuncWithOpaqueReturnTypeQryF',
                       '_$s14lazy_typecheck30publicFuncWithOpaqueReturnTypeQryFQOMQ',
                       '_$s14lazy_typecheck32constrainedGenericPublicFunctionyyxAA0E5ProtoRzlF',
                       '_$sSS14lazy_typecheck11PublicProtoAAMc', '_$sSS14lazy_typecheck11PublicProtoAAWP',
                       '_$sSS14lazy_typecheckE3reqSiyF', '_$sSi14lazy_typecheck19PublicRethrowsProtoAAMc',
                       '_$sSi14lazy_typecheck19PublicRethrowsProtoAAWP', '_$sSi14lazy_typecheckE3reqSiyKF' ]
...
