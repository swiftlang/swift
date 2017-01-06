//===--- DictionaryKeys.h - -------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_SOURCEKITD_LIB_API_DICTIONARYKEYS_H
#define LLVM_SOURCEKITD_LIB_API_DICTIONARYKEYS_H

namespace SourceKit {
  class UIdent;
}

namespace sourcekitd {

extern SourceKit::UIdent KeyVersionMajor;
extern SourceKit::UIdent KeyVersionMinor;
extern SourceKit::UIdent KeyResults;
extern SourceKit::UIdent KeyRequest;
extern SourceKit::UIdent KeyCompilerArgs;
extern SourceKit::UIdent KeyOffset;
extern SourceKit::UIdent KeySourceFile;
extern SourceKit::UIdent KeySourceText;
extern SourceKit::UIdent KeyModuleName;
extern SourceKit::UIdent KeyGroupName;
extern SourceKit::UIdent KeyActionName;
extern SourceKit::UIdent KeySynthesizedExtension;
extern SourceKit::UIdent KeyNotification;
extern SourceKit::UIdent KeyKeyword;
extern SourceKit::UIdent KeyName;
extern SourceKit::UIdent KeyNames;
extern SourceKit::UIdent KeyUIDs;
extern SourceKit::UIdent KeyEnableSyntaxMap;
extern SourceKit::UIdent KeyEnableDiagnostics;
extern SourceKit::UIdent KeySyntacticOnly;
extern SourceKit::UIdent KeyLength;
extern SourceKit::UIdent KeyActionable;
extern SourceKit::UIdent KeyKind;
extern SourceKit::UIdent KeyAccessibility;
extern SourceKit::UIdent KeySetterAccessibility;
extern SourceKit::UIdent KeyUSR;
extern SourceKit::UIdent KeyOriginalUSR;
extern SourceKit::UIdent KeyDefaultImplementationOf;
extern SourceKit::UIdent KeyInterestedUSR;
extern SourceKit::UIdent KeyLine;
extern SourceKit::UIdent KeyColumn;
extern SourceKit::UIdent KeyReceiverUSR;
extern SourceKit::UIdent KeyIsDynamic;
extern SourceKit::UIdent KeyIsTestCandidate;
extern SourceKit::UIdent KeyDescription;
extern SourceKit::UIdent KeyTypeName;
extern SourceKit::UIdent KeyRuntimeName;
extern SourceKit::UIdent KeySelectorName;
extern SourceKit::UIdent KeyOverrides;
extern SourceKit::UIdent KeyDocBrief;
extern SourceKit::UIdent KeyAssociatedUSRs;
extern SourceKit::UIdent KeyDocFullAsXML;
extern SourceKit::UIdent KeyGenericParams;
extern SourceKit::UIdent KeyGenericRequirements;
extern SourceKit::UIdent KeyAnnotatedDecl;
extern SourceKit::UIdent KeyFullyAnnotatedDecl;
extern SourceKit::UIdent KeyRelatedDecls;
extern SourceKit::UIdent KeyContext;
extern SourceKit::UIdent KeyModuleImportDepth;
extern SourceKit::UIdent KeyNumBytesToErase;
extern SourceKit::UIdent KeyNotRecommended;
extern SourceKit::UIdent KeyFilePath;
extern SourceKit::UIdent KeyModuleInterfaceName;
extern SourceKit::UIdent KeyHash;
extern SourceKit::UIdent KeyRelated;
extern SourceKit::UIdent KeyInherits;
extern SourceKit::UIdent KeyConforms;
extern SourceKit::UIdent KeyExtends;
extern SourceKit::UIdent KeyDependencies;
extern SourceKit::UIdent KeyEntities;
extern SourceKit::UIdent KeyDiagnostics;
extern SourceKit::UIdent KeySeverity;
extern SourceKit::UIdent KeyRanges;
extern SourceKit::UIdent KeyFixits;
extern SourceKit::UIdent KeyAnnotations;
extern SourceKit::UIdent KeyDiagnosticStage;
extern SourceKit::UIdent KeySyntaxMap;
extern SourceKit::UIdent KeyIsSystem;
extern SourceKit::UIdent KeyEnableStructure;
extern SourceKit::UIdent KeySubStructure;
extern SourceKit::UIdent KeyElements;
extern SourceKit::UIdent KeyNameOffset;
extern SourceKit::UIdent KeyNameLength;
extern SourceKit::UIdent KeyBodyOffset;
extern SourceKit::UIdent KeyBodyLength;
extern SourceKit::UIdent KeyThrowOffset;
extern SourceKit::UIdent KeyThrowLength;
extern SourceKit::UIdent KeyIsLocal;
extern SourceKit::UIdent KeyAttributes;
extern SourceKit::UIdent KeyAttribute;
extern SourceKit::UIdent KeyInheritedTypes;
extern SourceKit::UIdent KeyFormatOptions;
extern SourceKit::UIdent KeyCodeCompleteOptions;
extern SourceKit::UIdent KeyFilterRules;
extern SourceKit::UIdent KeyNextRequestStart;
extern SourceKit::UIdent KeyPopular;
extern SourceKit::UIdent KeyUnpopular;
extern SourceKit::UIdent KeyHide;
extern SourceKit::UIdent KeySimplified;

extern SourceKit::UIdent KeyIsUnavailable;
extern SourceKit::UIdent KeyIsDeprecated;
extern SourceKit::UIdent KeyIsOptional;
extern SourceKit::UIdent KeyPlatform;
extern SourceKit::UIdent KeyMessage;
extern SourceKit::UIdent KeyIntroduced;
extern SourceKit::UIdent KeyDeprecated;
extern SourceKit::UIdent KeyObsoleted;
extern SourceKit::UIdent KeyRemoveCache;
extern SourceKit::UIdent KeyTypeInterface;
extern SourceKit::UIdent KeyTypeUsr;
extern SourceKit::UIdent KeyContainerTypeUsr;
extern SourceKit::UIdent KeyModuleGroups;

extern SourceKit::UIdent KeyRangeContent;
/// \brief Used for determining the printing order of dictionary keys.
bool compareDictKeys(SourceKit::UIdent LHS, SourceKit::UIdent RHS);

}

#endif
