//===-- ARMTargetParser - Parser for ARM target features --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements a target parser to recognise ARM hardware features
// such as FPU/CPU/ARCH/extensions and specific support such as HWDIV.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/ARMTargetParser.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Triple.h"
#include <cctype>

using namespace llvm;

static StringRef getHWDivSynonym(StringRef HWDiv) {
  return StringSwitch<StringRef>(HWDiv)
      .Case("thumb,arm", "arm,thumb")
      .Default(HWDiv);
}

// Allows partial match, ex. "v7a" matches "armv7a".
ARM::ArchKind ARM::parseArch(StringRef Arch) {
  Arch = getCanonicalArchName(Arch);
  StringRef Syn = getArchSynonym(Arch);
  for (const auto &A : ARCHNames) {
    if (A.getName().endswith(Syn))
      return A.ID;
  }
  return ArchKind::INVALID;
}

// Version number (ex. v7 = 7).
unsigned ARM::parseArchVersion(StringRef Arch) {
  Arch = getCanonicalArchName(Arch);
  switch (parseArch(Arch)) {
  case ArchKind::ARMV2:
  case ArchKind::ARMV2A:
    return 2;
  case ArchKind::ARMV3:
  case ArchKind::ARMV3M:
    return 3;
  case ArchKind::ARMV4:
  case ArchKind::ARMV4T:
    return 4;
  case ArchKind::ARMV5T:
  case ArchKind::ARMV5TE:
  case ArchKind::IWMMXT:
  case ArchKind::IWMMXT2:
  case ArchKind::XSCALE:
  case ArchKind::ARMV5TEJ:
    return 5;
  case ArchKind::ARMV6:
  case ArchKind::ARMV6K:
  case ArchKind::ARMV6T2:
  case ArchKind::ARMV6KZ:
  case ArchKind::ARMV6M:
    return 6;
  case ArchKind::ARMV7A:
  case ArchKind::ARMV7VE:
  case ArchKind::ARMV7R:
  case ArchKind::ARMV7M:
  case ArchKind::ARMV7S:
  case ArchKind::ARMV7EM:
  case ArchKind::ARMV7K:
    return 7;
  case ArchKind::ARMV8A:
  case ArchKind::ARMV8_1A:
  case ArchKind::ARMV8_2A:
  case ArchKind::ARMV8_3A:
  case ArchKind::ARMV8_4A:
  case ArchKind::ARMV8_5A:
  case ArchKind::ARMV8_6A:
  case ArchKind::ARMV8R:
  case ArchKind::ARMV8MBaseline:
  case ArchKind::ARMV8MMainline:
  case ArchKind::ARMV8_1MMainline:
    return 8;
  case ArchKind::INVALID:
    return 0;
  }
  llvm_unreachable("Unhandled architecture");
}

// Profile A/R/M
ARM::ProfileKind ARM::parseArchProfile(StringRef Arch) {
  Arch = getCanonicalArchName(Arch);
  switch (parseArch(Arch)) {
  case ArchKind::ARMV6M:
  case ArchKind::ARMV7M:
  case ArchKind::ARMV7EM:
  case ArchKind::ARMV8MMainline:
  case ArchKind::ARMV8MBaseline:
  case ArchKind::ARMV8_1MMainline:
    return ProfileKind::M;
  case ArchKind::ARMV7R:
  case ArchKind::ARMV8R:
    return ProfileKind::R;
  case ArchKind::ARMV7A:
  case ArchKind::ARMV7VE:
  case ArchKind::ARMV7K:
  case ArchKind::ARMV8A:
  case ArchKind::ARMV8_1A:
  case ArchKind::ARMV8_2A:
  case ArchKind::ARMV8_3A:
  case ArchKind::ARMV8_4A:
  case ArchKind::ARMV8_5A:
  case ArchKind::ARMV8_6A:
    return ProfileKind::A;
  case ArchKind::ARMV2:
  case ArchKind::ARMV2A:
  case ArchKind::ARMV3:
  case ArchKind::ARMV3M:
  case ArchKind::ARMV4:
  case ArchKind::ARMV4T:
  case ArchKind::ARMV5T:
  case ArchKind::ARMV5TE:
  case ArchKind::ARMV5TEJ:
  case ArchKind::ARMV6:
  case ArchKind::ARMV6K:
  case ArchKind::ARMV6T2:
  case ArchKind::ARMV6KZ:
  case ArchKind::ARMV7S:
  case ArchKind::IWMMXT:
  case ArchKind::IWMMXT2:
  case ArchKind::XSCALE:
  case ArchKind::INVALID:
    return ProfileKind::INVALID;
  }
  llvm_unreachable("Unhandled architecture");
}

StringRef ARM::getArchSynonym(StringRef Arch) {
  return StringSwitch<StringRef>(Arch)
      .Case("v5", "v5t")
      .Case("v5e", "v5te")
      .Case("v6j", "v6")
      .Case("v6hl", "v6k")
      .Cases("v6m", "v6sm", "v6s-m", "v6-m")
      .Cases("v6z", "v6zk", "v6kz")
      .Cases("v7", "v7a", "v7hl", "v7l", "v7-a")
      .Case("v7r", "v7-r")
      .Case("v7m", "v7-m")
      .Case("v7em", "v7e-m")
      .Cases("v8", "v8a", "v8l", "aarch64", "arm64", "v8-a")
      .Case("v8.1a", "v8.1-a")
      .Case("v8.2a", "v8.2-a")
      .Case("v8.3a", "v8.3-a")
      .Case("v8.4a", "v8.4-a")
      .Case("v8.5a", "v8.5-a")
      .Case("v8.6a", "v8.6-a")
      .Case("v8r", "v8-r")
      .Case("v8m.base", "v8-m.base")
      .Case("v8m.main", "v8-m.main")
      .Case("v8.1m.main", "v8.1-m.main")
      .Default(Arch);
}

bool ARM::getFPUFeatures(unsigned FPUKind, std::vector<StringRef> &Features) {

  if (FPUKind >= FK_LAST || FPUKind == FK_INVALID)
    return false;

  static const struct FPUFeatureNameInfo {
    const char *PlusName, *MinusName;
    FPUVersion MinVersion;
    FPURestriction MaxRestriction;
  } FPUFeatureInfoList[] = {
    // We have to specify the + and - versions of the name in full so
    // that we can return them as static StringRefs.
    //
    // Also, the SubtargetFeatures ending in just "sp" are listed here
    // under FPURestriction::None, which is the only FPURestriction in
    // which they would be valid (since FPURestriction::SP doesn't
    // exist).
    {"+vfp2", "-vfp2", FPUVersion::VFPV2, FPURestriction::D16},
    {"+vfp2sp", "-vfp2sp", FPUVersion::VFPV2, FPURestriction::SP_D16},
    {"+vfp3", "-vfp3", FPUVersion::VFPV3, FPURestriction::None},
    {"+vfp3d16", "-vfp3d16", FPUVersion::VFPV3, FPURestriction::D16},
    {"+vfp3d16sp", "-vfp3d16sp", FPUVersion::VFPV3, FPURestriction::SP_D16},
    {"+vfp3sp", "-vfp3sp", FPUVersion::VFPV3, FPURestriction::None},
    {"+fp16", "-fp16", FPUVersion::VFPV3_FP16, FPURestriction::SP_D16},
    {"+vfp4", "-vfp4", FPUVersion::VFPV4, FPURestriction::None},
    {"+vfp4d16", "-vfp4d16", FPUVersion::VFPV4, FPURestriction::D16},
    {"+vfp4d16sp", "-vfp4d16sp", FPUVersion::VFPV4, FPURestriction::SP_D16},
    {"+vfp4sp", "-vfp4sp", FPUVersion::VFPV4, FPURestriction::None},
    {"+fp-armv8", "-fp-armv8", FPUVersion::VFPV5, FPURestriction::None},
    {"+fp-armv8d16", "-fp-armv8d16", FPUVersion::VFPV5, FPURestriction::D16},
    {"+fp-armv8d16sp", "-fp-armv8d16sp", FPUVersion::VFPV5, FPURestriction::SP_D16},
    {"+fp-armv8sp", "-fp-armv8sp", FPUVersion::VFPV5, FPURestriction::None},
    {"+fullfp16", "-fullfp16", FPUVersion::VFPV5_FULLFP16, FPURestriction::SP_D16},
    {"+fp64", "-fp64", FPUVersion::VFPV2, FPURestriction::D16},
    {"+d32", "-d32", FPUVersion::VFPV3, FPURestriction::None},
  };

  for (const auto &Info: FPUFeatureInfoList) {
    if (FPUNames[FPUKind].FPUVer >= Info.MinVersion &&
        FPUNames[FPUKind].Restriction <= Info.MaxRestriction)
      Features.push_back(Info.PlusName);
    else
      Features.push_back(Info.MinusName);
  }

  static const struct NeonFeatureNameInfo {
    const char *PlusName, *MinusName;
    NeonSupportLevel MinSupportLevel;
  } NeonFeatureInfoList[] = {
    {"+neon", "-neon", NeonSupportLevel::Neon},
    {"+crypto", "-crypto", NeonSupportLevel::Crypto},
  };

  for (const auto &Info: NeonFeatureInfoList) {
    if (FPUNames[FPUKind].NeonSupport >= Info.MinSupportLevel)
      Features.push_back(Info.PlusName);
    else
      Features.push_back(Info.MinusName);
  }

  return true;
}

// Little/Big endian
ARM::EndianKind ARM::parseArchEndian(StringRef Arch) {
  if (Arch.startswith("armeb") || Arch.startswith("thumbeb") ||
      Arch.startswith("aarch64_be"))
    return EndianKind::BIG;

  if (Arch.startswith("arm") || Arch.startswith("thumb")) {
    if (Arch.endswith("eb"))
      return EndianKind::BIG;
    else
      return EndianKind::LITTLE;
  }

  if (Arch.startswith("aarch64") || Arch.startswith("aarch64_32"))
    return EndianKind::LITTLE;

  return EndianKind::INVALID;
}

// ARM, Thumb, AArch64
ARM::ISAKind ARM::parseArchISA(StringRef Arch) {
  return StringSwitch<ISAKind>(Arch)
      .StartsWith("aarch64", ISAKind::AARCH64)
      .StartsWith("arm64", ISAKind::AARCH64)
      .StartsWith("thumb", ISAKind::THUMB)
      .StartsWith("arm", ISAKind::ARM)
      .Default(ISAKind::INVALID);
}

unsigned ARM::parseFPU(StringRef FPU) {
  StringRef Syn = getFPUSynonym(FPU);
  for (const auto F : FPUNames) {
    if (Syn == F.getName())
      return F.ID;
  }
  return FK_INVALID;
}

ARM::NeonSupportLevel ARM::getFPUNeonSupportLevel(unsigned FPUKind) {
  if (FPUKind >= FK_LAST)
    return NeonSupportLevel::None;
  return FPUNames[FPUKind].NeonSupport;
}

// MArch is expected to be of the form (arm|thumb)?(eb)?(v.+)?(eb)?, but
// (iwmmxt|xscale)(eb)? is also permitted. If the former, return
// "v.+", if the latter, return unmodified string, minus 'eb'.
// If invalid, return empty string.
StringRef ARM::getCanonicalArchName(StringRef Arch) {
  size_t offset = StringRef::npos;
  StringRef A = Arch;
  StringRef Error = "";

  // Begins with "arm" / "thumb", move past it.
  if (A.startswith("arm64_32"))
    offset = 8;
  else if (A.startswith("arm64"))
    offset = 5;
  else if (A.startswith("aarch64_32"))
    offset = 10;
  else if (A.startswith("arm"))
    offset = 3;
  else if (A.startswith("thumb"))
    offset = 5;
  else if (A.startswith("aarch64")) {
    offset = 7;
    // AArch64 uses "_be", not "eb" suffix.
    if (A.find("eb") != StringRef::npos)
      return Error;
    if (A.substr(offset, 3) == "_be")
      offset += 3;
  }

  // Ex. "armebv7", move past the "eb".
  if (offset != StringRef::npos && A.substr(offset, 2) == "eb")
    offset += 2;
  // Or, if it ends with eb ("armv7eb"), chop it off.
  else if (A.endswith("eb"))
    A = A.substr(0, A.size() - 2);
  // Trim the head
  if (offset != StringRef::npos)
    A = A.substr(offset);

  // Empty string means offset reached the end, which means it's valid.
  if (A.empty())
    return Arch;

  // Only match non-marketing names
  if (offset != StringRef::npos) {
    // Must start with 'vN'.
    if (A.size() >= 2 && (A[0] != 'v' || !std::isdigit(A[1])))
      return Error;
    // Can't have an extra 'eb'.
    if (A.find("eb") != StringRef::npos)
      return Error;
  }

  // Arch will either be a 'v' name (v7a) or a marketing name (xscale).
  return A;
}

StringRef ARM::getFPUSynonym(StringRef FPU) {
  return StringSwitch<StringRef>(FPU)
      .Cases("fpa", "fpe2", "fpe3", "maverick", "invalid") // Unsupported
      .Case("vfp2", "vfpv2")
      .Case("vfp3", "vfpv3")
      .Case("vfp4", "vfpv4")
      .Case("vfp3-d16", "vfpv3-d16")
      .Case("vfp4-d16", "vfpv4-d16")
      .Cases("fp4-sp-d16", "vfpv4-sp-d16", "fpv4-sp-d16")
      .Cases("fp4-dp-d16", "fpv4-dp-d16", "vfpv4-d16")
      .Case("fp5-sp-d16", "fpv5-sp-d16")
      .Cases("fp5-dp-d16", "fpv5-dp-d16", "fpv5-d16")
      // FIXME: Clang uses it, but it's bogus, since neon defaults to vfpv3.
      .Case("neon-vfpv3", "neon")
      .Default(FPU);
}

StringRef ARM::getFPUName(unsigned FPUKind) {
  if (FPUKind >= FK_LAST)
    return StringRef();
  return FPUNames[FPUKind].getName();
}

ARM::FPUVersion ARM::getFPUVersion(unsigned FPUKind) {
  if (FPUKind >= FK_LAST)
    return FPUVersion::NONE;
  return FPUNames[FPUKind].FPUVer;
}

ARM::FPURestriction ARM::getFPURestriction(unsigned FPUKind) {
  if (FPUKind >= FK_LAST)
    return FPURestriction::None;
  return FPUNames[FPUKind].Restriction;
}

unsigned ARM::getDefaultFPU(StringRef CPU, ARM::ArchKind AK) {
  if (CPU == "generic")
    return ARM::ARCHNames[static_cast<unsigned>(AK)].DefaultFPU;

  return StringSwitch<unsigned>(CPU)
#define ARM_CPU_NAME(NAME, ID, DEFAULT_FPU, IS_DEFAULT, DEFAULT_EXT)           \
  .Case(NAME, DEFAULT_FPU)
#include "llvm/Support/ARMTargetParser.def"
   .Default(ARM::FK_INVALID);
}

uint64_t ARM::getDefaultExtensions(StringRef CPU, ARM::ArchKind AK) {
  if (CPU == "generic")
    return ARM::ARCHNames[static_cast<unsigned>(AK)].ArchBaseExtensions;

  return StringSwitch<uint64_t>(CPU)
#define ARM_CPU_NAME(NAME, ID, DEFAULT_FPU, IS_DEFAULT, DEFAULT_EXT)           \
  .Case(NAME,                                                                  \
        ARCHNames[static_cast<unsigned>(ArchKind::ID)].ArchBaseExtensions |    \
            DEFAULT_EXT)
#include "llvm/Support/ARMTargetParser.def"
  .Default(ARM::AEK_INVALID);
}

bool ARM::getHWDivFeatures(uint64_t HWDivKind,
                           std::vector<StringRef> &Features) {

  if (HWDivKind == AEK_INVALID)
    return false;

  if (HWDivKind & AEK_HWDIVARM)
    Features.push_back("+hwdiv-arm");
  else
    Features.push_back("-hwdiv-arm");

  if (HWDivKind & AEK_HWDIVTHUMB)
    Features.push_back("+hwdiv");
  else
    Features.push_back("-hwdiv");

  return true;
}

bool ARM::getExtensionFeatures(uint64_t Extensions,
                               std::vector<StringRef> &Features) {

  if (Extensions == AEK_INVALID)
    return false;

  for (const auto AE : ARCHExtNames) {
    if ((Extensions & AE.ID) == AE.ID && AE.Feature)
      Features.push_back(AE.Feature);
    else if (AE.NegFeature)
      Features.push_back(AE.NegFeature);
  }

  return getHWDivFeatures(Extensions, Features);
}

StringRef ARM::getArchName(ARM::ArchKind AK) {
  return ARCHNames[static_cast<unsigned>(AK)].getName();
}

StringRef ARM::getCPUAttr(ARM::ArchKind AK) {
  return ARCHNames[static_cast<unsigned>(AK)].getCPUAttr();
}

StringRef ARM::getSubArch(ARM::ArchKind AK) {
  return ARCHNames[static_cast<unsigned>(AK)].getSubArch();
}

unsigned ARM::getArchAttr(ARM::ArchKind AK) {
  return ARCHNames[static_cast<unsigned>(AK)].ArchAttr;
}

StringRef ARM::getArchExtName(uint64_t ArchExtKind) {
  for (const auto AE : ARCHExtNames) {
    if (ArchExtKind == AE.ID)
      return AE.getName();
  }
  return StringRef();
}

static bool stripNegationPrefix(StringRef &Name) {
  if (Name.startswith("no")) {
    Name = Name.substr(2);
    return true;
  }
  return false;
}

StringRef ARM::getArchExtFeature(StringRef ArchExt) {
  bool Negated = stripNegationPrefix(ArchExt);
  for (const auto AE : ARCHExtNames) {
    if (AE.Feature && ArchExt == AE.getName())
      return StringRef(Negated ? AE.NegFeature : AE.Feature);
  }

  return StringRef();
}

static unsigned findDoublePrecisionFPU(unsigned InputFPUKind) {
  const ARM::FPUName &InputFPU = ARM::FPUNames[InputFPUKind];

  // If the input FPU already supports double-precision, then there
  // isn't any different FPU we can return here.
  //
  // The current available FPURestriction values are None (no
  // restriction), D16 (only 16 d-regs) and SP_D16 (16 d-regs
  // and single precision only); there's no value representing
  // SP restriction without D16. So this test just means 'is it
  // SP only?'.
  if (InputFPU.Restriction != ARM::FPURestriction::SP_D16)
    return ARM::FK_INVALID;

  // Otherwise, look for an FPU entry with all the same fields, except
  // that SP_D16 has been replaced with just D16, representing adding
  // double precision and not changing anything else.
  for (const ARM::FPUName &CandidateFPU : ARM::FPUNames) {
    if (CandidateFPU.FPUVer == InputFPU.FPUVer &&
        CandidateFPU.NeonSupport == InputFPU.NeonSupport &&
        CandidateFPU.Restriction == ARM::FPURestriction::D16) {
      return CandidateFPU.ID;
    }
  }

  // nothing found
  return ARM::FK_INVALID;
}

bool ARM::appendArchExtFeatures(
  StringRef CPU, ARM::ArchKind AK, StringRef ArchExt,
  std::vector<StringRef> &Features) {

  size_t StartingNumFeatures = Features.size();
  const bool Negated = stripNegationPrefix(ArchExt);
  uint64_t ID = parseArchExt(ArchExt);

  if (ID == AEK_INVALID)
    return false;

  for (const auto AE : ARCHExtNames) {
    if (Negated) {
      if ((AE.ID & ID) == ID && AE.NegFeature)
        Features.push_back(AE.NegFeature);
    } else {
      if ((AE.ID & ID) == AE.ID && AE.Feature)
        Features.push_back(AE.Feature);
    }
  }

  if (CPU == "")
    CPU = "generic";

  if (ArchExt == "fp" || ArchExt == "fp.dp") {
    unsigned FPUKind;
    if (ArchExt == "fp.dp") {
      if (Negated) {
        Features.push_back("-fp64");
        return true;
      }
      FPUKind = findDoublePrecisionFPU(getDefaultFPU(CPU, AK));
    } else if (Negated) {
      FPUKind = ARM::FK_NONE;
    } else {
      FPUKind = getDefaultFPU(CPU, AK);
    }
    return ARM::getFPUFeatures(FPUKind, Features);
  }
  return StartingNumFeatures != Features.size();
}

StringRef ARM::getHWDivName(uint64_t HWDivKind) {
  for (const auto D : HWDivNames) {
    if (HWDivKind == D.ID)
      return D.getName();
  }
  return StringRef();
}

StringRef ARM::getDefaultCPU(StringRef Arch) {
  ArchKind AK = parseArch(Arch);
  if (AK == ArchKind::INVALID)
    return StringRef();

  // Look for multiple AKs to find the default for pair AK+Name.
  for (const auto CPU : CPUNames) {
    if (CPU.ArchID == AK && CPU.Default)
      return CPU.getName();
  }

  // If we can't find a default then target the architecture instead
  return "generic";
}

uint64_t ARM::parseHWDiv(StringRef HWDiv) {
  StringRef Syn = getHWDivSynonym(HWDiv);
  for (const auto D : HWDivNames) {
    if (Syn == D.getName())
      return D.ID;
  }
  return AEK_INVALID;
}

uint64_t ARM::parseArchExt(StringRef ArchExt) {
  for (const auto A : ARCHExtNames) {
    if (ArchExt == A.getName())
      return A.ID;
  }
  return AEK_INVALID;
}

ARM::ArchKind ARM::parseCPUArch(StringRef CPU) {
  for (const auto C : CPUNames) {
    if (CPU == C.getName())
      return C.ArchID;
  }
  return ArchKind::INVALID;
}

void ARM::fillValidCPUArchList(SmallVectorImpl<StringRef> &Values) {
  for (const CpuNames<ArchKind> &Arch : CPUNames) {
    if (Arch.ArchID != ArchKind::INVALID)
      Values.push_back(Arch.getName());
  }
}

StringRef ARM::computeDefaultTargetABI(const Triple &TT, StringRef CPU) {
  StringRef ArchName =
      CPU.empty() ? TT.getArchName() : getArchName(parseCPUArch(CPU));

  if (TT.isOSBinFormatMachO()) {
    if (TT.getEnvironment() == Triple::EABI ||
        TT.getOS() == Triple::UnknownOS ||
        parseArchProfile(ArchName) == ProfileKind::M)
      return "aapcs";
    if (TT.isWatchABI())
      return "aapcs16";
    return "apcs-gnu";
  } else if (TT.isOSWindows())
    // FIXME: this is invalid for WindowsCE.
    return "aapcs";

  // Select the default based on the platform.
  switch (TT.getEnvironment()) {
  case Triple::Android:
  case Triple::GNUEABI:
  case Triple::GNUEABIHF:
  case Triple::MuslEABI:
  case Triple::MuslEABIHF:
    return "aapcs-linux";
  case Triple::EABIHF:
  case Triple::EABI:
    return "aapcs";
  default:
    if (TT.isOSNetBSD())
      return "apcs-gnu";
    if (TT.isOSOpenBSD())
      return "aapcs-linux";
    return "aapcs";
  }
}
