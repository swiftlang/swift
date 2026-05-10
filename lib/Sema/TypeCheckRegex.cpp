//===--- TypeCheckRegex.cpp - Regex type checking utilities ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Bridging/ASTGen.h"

using namespace swift;

typedef uint16_t CaptureStructureSerializationVersion;

static unsigned
getCaptureStructureSerializationAllocationSize(unsigned regexLength) {
  return sizeof(CaptureStructureSerializationVersion) + regexLength + 1;
}

enum class RegexCaptureStructureCode: uint8_t {
  End           = 0,
  Atom          = 1,
  NamedAtom     = 2,
  FormArray     = 3,
  FormOptional  = 4,
  BeginTuple    = 5,
  EndTuple      = 6,
  CaseCount
};

/// Decodes regex capture types from the given serialization and appends the
/// decoded capture types to @p result. Returns true if the serialization is
/// malformed.
static bool decodeRegexCaptureTypes(ASTContext &ctx,
                                    ArrayRef<uint8_t> serialization,
                                    Type atomType,
                                    SmallVectorImpl<TupleTypeElt> &result) {
  // Encoding rules:
  // encode(〚`T`〛) ==> <version>, 〚`T`〛, .end
  // 〚`T` (atom)〛 ==> .atom
  // 〚`name: T` (atom)〛 ==> .atom, `name`, '\0'
  // 〚`[T]`〛 ==> 〚`T`〛, .formArray
  // 〚`T?`〛 ==> 〚`T`〛, .formOptional
  // 〚`(T0, T1, ...)` (top level)〛 ==> 〚`T0`〛, 〚`T1`〛, ...
  // 〚`(T0, T1, ...)`〛 ==> .beginTuple, 〚`T0`〛, 〚`T1`〛, ..., .endTuple
  //
  // For details, see apple/swift-experimental-string-processing.
  using Version = CaptureStructureSerializationVersion;
  static const Version implVersion = 1;
  unsigned size = serialization.size();
  // A serialization should store a version and `.end` at the very least.
  unsigned minSize = sizeof(Version) + sizeof(RegexCaptureStructureCode);
  if (size < minSize)
    return false;
  // Read version.
  Version version = *reinterpret_cast<const Version *>(serialization.data());
  if (version != implVersion)
    return true;
  // Read contents.
  SmallVector<SmallVector<TupleTypeElt, 4>, 4> scopes(1);
  unsigned offset = sizeof(Version);
  auto consumeCode = [&]() -> std::optional<RegexCaptureStructureCode> {
    auto rawValue = serialization[offset];
    if (rawValue >= (uint8_t)RegexCaptureStructureCode::CaseCount)
      return std::nullopt;
    offset += sizeof(RegexCaptureStructureCode);
    return (RegexCaptureStructureCode)rawValue;
  };
  do {
    auto code = consumeCode();
    if (!code)
      return false;
    switch (*code) {
    case RegexCaptureStructureCode::End:
      offset = size;
      break;
    case RegexCaptureStructureCode::Atom:
      scopes.back().push_back(atomType);
      break;
    case RegexCaptureStructureCode::NamedAtom: {
      auto *namePtr = reinterpret_cast<const char *>(
          serialization.slice(offset).data());
      auto length = strnlen(namePtr, size - offset);
      if (length >= size - offset)
        return true; // Unterminated string.
      StringRef name(namePtr, length);
      scopes.back().push_back(
          TupleTypeElt(atomType, ctx.getIdentifier(name)));
      offset += length + /*NUL*/ 1;
      break;
    }
    case RegexCaptureStructureCode::FormArray: {
      auto &element = scopes.back().back();
      element = TupleTypeElt(ArraySliceType::get(element.getType()),
                             element.getName());
      break;
    }
    case RegexCaptureStructureCode::FormOptional: {
      auto &element = scopes.back().back();
      element = TupleTypeElt(OptionalType::get(element.getType()),
                             element.getName());
      break;
    }
    case RegexCaptureStructureCode::BeginTuple:
      scopes.push_back({});
      break;
    case RegexCaptureStructureCode::EndTuple: {
      auto children = scopes.pop_back_val();
      assert(children.size() > 1);
      auto type = TupleType::get(children, ctx);
      scopes.back().push_back(Type(type));
      break;
    }
    case RegexCaptureStructureCode::CaseCount:
      llvm_unreachable("Handled earlier");
    }
  } while (offset < size);
  if (scopes.size() != 1)
    return true; // Unterminated tuple.
  auto &elements = scopes.back();
  result.append(elements.begin(), elements.end());
  return false;
}

static Type computeRegexLiteralType(const RegexLiteralExpr *regex,
                                    ArrayRef<uint8_t> serializedCaptures) {
  auto &ctx = regex->getASTContext();
  auto *regexDecl = ctx.getRegexDecl();
  if (!regexDecl) {
    ctx.Diags.diagnose(regex->getLoc(), diag::string_processing_lib_missing,
                       ctx.Id_Regex.str());
    return Type();
  }

  SmallVector<TupleTypeElt, 4> matchElements;
  if (decodeRegexCaptureTypes(ctx, serializedCaptures,
                              /*atomType*/ ctx.getSubstringType(),
                              matchElements)) {
    ctx.Diags.diagnose(regex->getLoc(),
                       diag::regex_capture_types_failed_to_decode);
    return Type();
  }
  assert(!matchElements.empty() && "Should have decoded at least an atom");
  if (matchElements.size() == 1)
    return BoundGenericStructType::get(regexDecl, Type(),
                                       matchElements.front().getType());
  // Form a tuple.
  auto matchType = TupleType::get(matchElements, ctx);
  return BoundGenericStructType::get(regexDecl, Type(), {matchType});
}

RegexLiteralPatternInfo
RegexLiteralPatternInfoRequest::evaluate(Evaluator &eval,
                                         const RegexLiteralExpr *regex) const {
#if SWIFT_BUILD_REGEX_PARSER_IN_COMPILER
  auto &ctx = regex->getASTContext();
  auto regexText = regex->getParsedRegexText();

  // Let the Swift library parse the contents, returning an error, or null if
  // successful.
  size_t version = 0;
  auto capturesSize =
      getCaptureStructureSerializationAllocationSize(regexText.size());
  std::vector<uint8_t> capturesBuf(capturesSize);

  BridgedRegexLiteralPatternFeatures bridgedFeatures;
  SWIFT_DEFER {
    swift_ASTGen_freeBridgedRegexLiteralPatternFeatures(bridgedFeatures);
  };

  bool hadError = swift_ASTGen_parseRegexLiteral(
      regexText,
      /*versionOut=*/&version,
      /*captureStructureOut=*/capturesBuf.data(),
      /*captureStructureSize=*/capturesBuf.size(),
      /*patternFeaturesOut=*/&bridgedFeatures,
      /*diagBaseLoc=*/regex->getLoc(), &ctx.Diags);
  if (hadError)
    return {regexText, Type(), /*version*/ 0, /*features*/ {}};

  SmallVector<RegexLiteralPatternFeature> features;
  for (auto &bridgedFeature : bridgedFeatures.unbridged())
    features.push_back(bridgedFeature.unbridged());

  assert(version >= 1);
  auto regexTy = computeRegexLiteralType(regex, capturesBuf);

  // FIXME: We need to plumb through the 'regexToEmit' result to the caller.
  // For now, it is the same as the input.
  return {/*regexToEmit*/ regexText, regexTy, version,
          ctx.AllocateCopy(features)};
#else
  llvm_unreachable("Shouldn't have parsed a RegexLiteralExpr");
#endif
}

StringRef RegexLiteralFeatureDescriptionRequest::evaluate(
    Evaluator &evaluator, RegexLiteralPatternFeatureKind kind,
    ASTContext *ctx) const {
#if SWIFT_BUILD_REGEX_PARSER_IN_COMPILER
  // The resulting string is allocated in the ASTContext, we can return the
  // StringRef directly.
  BridgedStringRef str;
  swift_ASTGen_getDescriptionForRegexPatternFeature(kind, *ctx, &str);
  return str.unbridged();
#else
  llvm_unreachable("Shouldn't have parsed a RegexLiteralExpr");
#endif
}

AvailabilityRange RegexLiteralFeatureAvailabilityRequest::evaluate(
    Evaluator &evaluator, RegexLiteralPatternFeatureKind kind,
    ASTContext *ctx) const {
#if SWIFT_BUILD_REGEX_PARSER_IN_COMPILER
  BridgedSwiftVersion version;
  swift_ASTGen_getSwiftVersionForRegexPatternFeature(kind, &version);
  return ctx->getSwiftAvailability(version.getMajor(), version.getMinor());
#else
  llvm_unreachable("Shouldn't have parsed a RegexLiteralExpr");
#endif
}
