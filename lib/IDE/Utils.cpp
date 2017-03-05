//===--- Utils.cpp - Misc utilities ---------------------------------------===//
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

#include "swift/IDE/Utils.h"
#include "swift/Basic/SourceManager.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Parse/Parser.h"
#include "swift/Subsystems.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclObjC.h"
#include "clang/CodeGen/ObjectFilePCHContainerOperations.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/TextDiagnosticBuffer.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Serialization/ASTReader.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"

using namespace swift;
using namespace ide;

static const char *skipStringInCode(const char *p, const char *End);

static const char *skipParenExpression(const char *p, const char *End) {
  const char *e = p;
  if (*e == '(') {
    uint32_t ParenCount = 1;
    bool done = false;
    for (++e; e < End; ++e) {
      switch (*e) {
      case ')':
        done = --ParenCount == 0;
        break;
                  
      case '(':
        ++ParenCount;
        break;
              
      case '"':
        e = skipStringInCode (e, End);
        break;
              
      default:
        break;
      }
      // If "done" is true make sure we don't increment "e"
      if (done)
        break;
    }
  }
  if (e >= End)
    return End;
  return e;
}

static const char *skipStringInCode(const char *p, const char *End) {
  const char *e = p;
  if (*e == '"') {
    bool done = false;
    for (++e; e < End; ++e) {
      switch (*e) {
      case '"':
        done = true;
        break;
                  
      case '\\':
        ++e;
        if (e >= End)
          done = true;
        else if (*e == '(')
          e = skipParenExpression (e, End);
        break;
              
      default:
        break;
      }
      // If "done" is true make sure we don't increment "e"
      if (done)
          break;
    }
  }
  if (e >= End)
    return End;
  return e;
}

SourceCompleteResult
ide::isSourceInputComplete(std::unique_ptr<llvm::MemoryBuffer> MemBuf) {
  SourceManager SM;
  auto BufferID = SM.addNewSourceBuffer(std::move(MemBuf));
  ParserUnit Parse(SM, BufferID);
  Parser &P = Parse.getParser();

  bool Done;
  do {
    P.parseTopLevel();
    Done = P.Tok.is(tok::eof);
  } while (!Done);

  SourceCompleteResult SCR;
  SCR.IsComplete = !P.isInputIncomplete();
    
  // Use the same code that was in the REPL code to track the indent level 
  // for now. In the future we should get this from the Parser if possible.

  CharSourceRange entireRange = SM.getRangeForBuffer(BufferID);
  StringRef Buffer = SM.extractText(entireRange);
  const char *SourceStart = Buffer.data();
  const char *SourceEnd = Buffer.data() + Buffer.size();
  const char *LineStart = SourceStart;
  const char *LineSourceStart = nullptr;
  uint32_t LineIndent = 0;
  struct IndentInfo {
    StringRef Prefix;
    uint32_t Indent;
    IndentInfo(const char *s, size_t n, uint32_t i) :
      Prefix(s, n),
      Indent(i) {}
  };
  SmallVector<IndentInfo, 4> IndentInfos;
  for (const char *p = SourceStart; p<SourceEnd; ++p) {
    switch (*p) {
    case '\r':
    case '\n':
      LineIndent = 0;
      LineSourceStart = nullptr;
      LineStart = p + 1;
      break;

    case '"':
      p = skipStringInCode (p, SourceEnd);
      break;

    case '{':
    case '(':
    case '[':
      ++LineIndent;
      if (LineSourceStart == nullptr)
        IndentInfos.push_back(IndentInfo(LineStart,
                                         p - LineStart,
                                         LineIndent));
      else
        IndentInfos.push_back(IndentInfo(LineStart,
                                         LineSourceStart - LineStart,
                                         LineIndent));
      break;

    case '}':
    case ')':
    case ']':
      if (LineIndent > 0)
        --LineIndent;
      if (!IndentInfos.empty())
        IndentInfos.pop_back();
      break;
  
    default:
      if (LineSourceStart == nullptr && !isspace(*p))
        LineSourceStart = p;
      break;
    }
    if (*p == '\0')
      break;
  }
  if (!IndentInfos.empty()) {
    SCR.IndentPrefix = IndentInfos.back().Prefix.str();
    // Trim off anything that follows a non-space character
    const size_t pos = SCR.IndentPrefix.find_first_not_of(" \t");
    if (pos != std::string::npos)
        SCR.IndentPrefix.erase(pos);
    SCR.IndentLevel = IndentInfos.back().Indent;
  }
  return SCR;
}

SourceCompleteResult ide::isSourceInputComplete(StringRef Text) {
  return ide::isSourceInputComplete(llvm::MemoryBuffer::getMemBufferCopy(Text));
}

// Adjust the cc1 triple string we got from clang, to make sure it will be
// accepted when it goes through the swift clang importer.
static std::string adjustClangTriple(StringRef TripleStr) {
  std::string Result;
  llvm::raw_string_ostream OS(Result);

  llvm::Triple Triple(TripleStr);
  switch (Triple.getSubArch()) {
  case llvm::Triple::SubArchType::ARMSubArch_v7:
    OS << "armv7"; break;
  case llvm::Triple::SubArchType::ARMSubArch_v7s:
    OS << "armv7s"; break;
  case llvm::Triple::SubArchType::ARMSubArch_v7k:
    OS << "armv7k"; break;
  case llvm::Triple::SubArchType::ARMSubArch_v6:
    OS << "armv6"; break;
  case llvm::Triple::SubArchType::ARMSubArch_v6m:
    OS << "armv6m"; break;
  case llvm::Triple::SubArchType::ARMSubArch_v6k:
    OS << "armv6k"; break;
  case llvm::Triple::SubArchType::ARMSubArch_v6t2:
    OS << "armv6t2"; break;
  default:
    // Adjust i386-macosx to x86_64 because there is no Swift stdlib for i386.
    if ((Triple.getOS() == llvm::Triple::MacOSX ||
      Triple.getOS() == llvm::Triple::Darwin) && Triple.getArch() == llvm::Triple::x86) {
      OS << "x86_64";
    } else {
      OS << Triple.getArchName();
    }
    break;
  }
  OS << '-' << Triple.getVendorName() << '-' << Triple.getOSName();
  OS.flush();
  return Result;
}

bool ide::initInvocationByClangArguments(ArrayRef<const char *> ArgList,
                                         CompilerInvocation &Invok,
                                         std::string &Error) {
  llvm::IntrusiveRefCntPtr<clang::DiagnosticOptions> DiagOpts{
    new clang::DiagnosticOptions()
  };

  clang::TextDiagnosticBuffer DiagBuf;
  llvm::IntrusiveRefCntPtr<clang::DiagnosticsEngine> ClangDiags =
    clang::CompilerInstance::createDiagnostics(DiagOpts.get(),
                                               &DiagBuf,
                                               /*ShouldOwnClient=*/false);

  // Clang expects this to be like an actual command line. So we need to pass in
  // "clang" for argv[0].
  std::vector<const char *> ClangArgList;
  ClangArgList.push_back("clang");
  ClangArgList.insert(ClangArgList.end(), ArgList.begin(), ArgList.end());

  // Create a new Clang compiler invocation.
  std::unique_ptr<clang::CompilerInvocation> ClangInvok =
      clang::createInvocationFromCommandLine(ClangArgList, ClangDiags);
  if (!ClangInvok || ClangDiags->hasErrorOccurred()) {
    for (auto I = DiagBuf.err_begin(), E = DiagBuf.err_end(); I != E; ++I) {
      Error += I->second;
      Error += " ";
    }
    return true;
  }

  auto &PPOpts = ClangInvok->getPreprocessorOpts();
  auto &HSOpts = ClangInvok->getHeaderSearchOpts();

  Invok.setTargetTriple(adjustClangTriple(ClangInvok->getTargetOpts().Triple));
  if (!HSOpts.Sysroot.empty())
    Invok.setSDKPath(HSOpts.Sysroot);
  if (!HSOpts.ModuleCachePath.empty())
    Invok.setClangModuleCachePath(HSOpts.ModuleCachePath);

  auto &CCArgs = Invok.getClangImporterOptions().ExtraArgs;
  for (auto MacroEntry : PPOpts.Macros) {
    std::string MacroFlag;
    if (MacroEntry.second)
      MacroFlag += "-U";
    else
      MacroFlag += "-D";
    MacroFlag += MacroEntry.first;
    CCArgs.push_back(MacroFlag);
  }

  for (auto &Entry : HSOpts.UserEntries) {
    switch (Entry.Group) {
      case clang::frontend::Quoted:
        CCArgs.push_back("-iquote");
        CCArgs.push_back(Entry.Path);
        break;
      case clang::frontend::IndexHeaderMap:
        CCArgs.push_back("-index-header-map");
        LLVM_FALLTHROUGH;
      case clang::frontend::Angled: {
        std::string Flag;
        if (Entry.IsFramework)
          Flag += "-F";
        else
          Flag += "-I";
        Flag += Entry.Path;
        CCArgs.push_back(Flag);
        break;
      }
      case clang::frontend::System:
        if (Entry.IsFramework)
          CCArgs.push_back("-iframework");
        else
          CCArgs.push_back("-isystem");
        CCArgs.push_back(Entry.Path);
        break;
      case clang::frontend::ExternCSystem:
      case clang::frontend::CSystem:
      case clang::frontend::CXXSystem:
      case clang::frontend::ObjCSystem:
      case clang::frontend::ObjCXXSystem:
      case clang::frontend::After:
        break;
    }
  }

  if (!PPOpts.ImplicitPCHInclude.empty()) {
    clang::FileSystemOptions FileSysOpts;
    clang::FileManager FileMgr(FileSysOpts);
    auto PCHContainerOperations =
        std::make_shared<clang::PCHContainerOperations>();
    std::string HeaderFile = clang::ASTReader::getOriginalSourceFile(
        PPOpts.ImplicitPCHInclude, FileMgr,
        PCHContainerOperations->getRawReader(), *ClangDiags);
    if (!HeaderFile.empty()) {
      CCArgs.push_back("-include");
      CCArgs.push_back(std::move(HeaderFile));
    }
  }
  for (auto &Header : PPOpts.Includes) {
    CCArgs.push_back("-include");
    CCArgs.push_back(Header);
  }

  for (auto &Entry : HSOpts.ModulesIgnoreMacros) {
    std::string Flag = "-fmodules-ignore-macro=";
    Flag += Entry;
    CCArgs.push_back(Flag);
  }

  for (auto &Entry : HSOpts.VFSOverlayFiles) {
    CCArgs.push_back("-ivfsoverlay");
    CCArgs.push_back(Entry);
  }

  if (!ClangInvok->getLangOpts()->isCompilingModule()) {
    CCArgs.push_back("-Xclang");
    llvm::SmallString<64> Str;
    Str += "-fmodule-name=";
    Str += ClangInvok->getLangOpts()->CurrentModule;
    CCArgs.push_back(Str.str());
  }

  if (PPOpts.DetailedRecord) {
    Invok.getClangImporterOptions().DetailedPreprocessingRecord = true;
  }

  if (!ClangInvok->getFrontendOpts().Inputs.empty()) {
    Invok.getFrontendOptions().ImplicitObjCHeaderPath =
      ClangInvok->getFrontendOpts().Inputs[0].getFile();
  }

  return false;
}

template <typename FnTy>
static void walkOverriddenClangDecls(const clang::NamedDecl *D, const FnTy &Fn){
  SmallVector<const clang::NamedDecl *, 8> OverDecls;
  D->getASTContext().getOverriddenMethods(D, OverDecls);
  for (auto Over : OverDecls)
    Fn(Over);
  for (auto Over : OverDecls)
    walkOverriddenClangDecls(Over, Fn);
}

void
ide::walkOverriddenDecls(const ValueDecl *VD,
                         std::function<void(llvm::PointerUnion<
                             const ValueDecl*, const clang::NamedDecl*>)> Fn) {
  for (auto CurrOver = VD; CurrOver; CurrOver = CurrOver->getOverriddenDecl()) {
    if (CurrOver != VD)
      Fn(CurrOver);
    if (auto ClangD =
        dyn_cast_or_null<clang::NamedDecl>(CurrOver->getClangDecl())) {
      walkOverriddenClangDecls(ClangD, Fn);
      return;
    }
    for (auto Conf: CurrOver->getSatisfiedProtocolRequirements())
      Fn(Conf);
  }
}

/// \returns true if a placeholder was found.
static bool findPlaceholder(StringRef Input, PlaceholderOccurrence &Occur) {
  while (true) {
    size_t Pos = Input.find("<#");
    if (Pos == StringRef::npos)
      return false;

    const char *Begin = Input.begin() + Pos;
    const char *Ptr = Begin + 2;
    const char *End = Input.end();
    for (; Ptr < End-1; ++Ptr) {
      if (*Ptr == '\n')
        break;
      if (Ptr[0] == '<' && Ptr[1] == '#')
        break;
      if (Ptr[0] == '#' && Ptr[1] == '>') {
        // Found it.
        Occur.FullPlaceholder = Input.substr(Pos, Ptr-Begin + 2);
        Occur.PlaceholderContent =
          Occur.FullPlaceholder.drop_front(2).drop_back(2);
        return true;
      }
    }

    // Try again.
    Input = Input.substr(Ptr - Input.begin());
  }
}

std::unique_ptr<llvm::MemoryBuffer>
ide::replacePlaceholders(std::unique_ptr<llvm::MemoryBuffer> InputBuf,
             llvm::function_ref<void(const PlaceholderOccurrence &)> Callback) {
  StringRef Input = InputBuf->getBuffer();
  PlaceholderOccurrence Occur;
  bool Found = findPlaceholder(Input, Occur);
  if (!Found)
    return InputBuf;

  std::unique_ptr<llvm::MemoryBuffer> NewBuf =
    llvm::MemoryBuffer::getMemBufferCopy(InputBuf->getBuffer(),
                                         InputBuf->getBufferIdentifier());

  unsigned Counter = 0;
  auto replacePlaceholder = [&](PlaceholderOccurrence &Occur) {
    llvm::SmallString<10> Id;
    Id = "$_";
    llvm::raw_svector_ostream(Id) << (Counter++);
    assert(Occur.FullPlaceholder.size() >= 2);
    if (Id.size() > Occur.FullPlaceholder.size()) {
      // The identifier+counter exceeds placeholder size; replace it without
      // using the counter.
      Id = "$";
      Id.append(Occur.FullPlaceholder.size()-1, '_');
    } else {
      Id.append(Occur.FullPlaceholder.size()-Id.size(), '_');
    }
    assert(Id.size() == Occur.FullPlaceholder.size());

    unsigned Offset = Occur.FullPlaceholder.data() - InputBuf->getBufferStart();
    char *Ptr = (char*)NewBuf->getBufferStart() + Offset;
    std::copy(Id.begin(), Id.end(), Ptr);

    Occur.IdentifierReplacement = Id.str();
    Callback(Occur);
  };

  while (true) {
    replacePlaceholder(Occur);
    unsigned Offset = Occur.FullPlaceholder.data() - InputBuf->getBufferStart();
    Input = InputBuf->getBuffer().substr(Offset+Occur.FullPlaceholder.size());

    bool Found = findPlaceholder(Input, Occur);
    if (!Found)
      break;
  }

  return NewBuf;
}

std::unique_ptr<llvm::MemoryBuffer>
ide::replacePlaceholders(std::unique_ptr<llvm::MemoryBuffer> InputBuf,
                         bool *HadPlaceholder) {
  if (HadPlaceholder)
    *HadPlaceholder = false;
  return replacePlaceholders(std::move(InputBuf),
                             [&](const PlaceholderOccurrence &){
    if (HadPlaceholder)
      *HadPlaceholder = true;
  });
}

static std::string getPlistEntry(const llvm::Twine &Path, StringRef KeyName) {
  auto BufOrErr = llvm::MemoryBuffer::getFile(Path);
  if (!BufOrErr) {
    llvm::errs() << BufOrErr.getError().message() << '\n';
    return {};
  }

  std::string Key = "<key>";
  Key += KeyName;
  Key += "</key>";

  StringRef Lines = BufOrErr.get()->getBuffer();
  while (!Lines.empty()) {
    StringRef CurLine;
    std::tie(CurLine, Lines) = Lines.split('\n');
    if (CurLine.find(Key) != StringRef::npos) {
      std::tie(CurLine, Lines) = Lines.split('\n');
      unsigned Begin = CurLine.find("<string>") + strlen("<string>");
      unsigned End = CurLine.find("</string>");
      return CurLine.substr(Begin, End-Begin);
    }
  }

  return {};
}

std::string ide::getSDKName(StringRef Path) {
  std::string Name = getPlistEntry(llvm::Twine(Path)+"/SDKSettings.plist",
                                   "CanonicalName");
  if (Name.empty() && Path.endswith(".sdk")) {
    Name = llvm::sys::path::filename(Path).drop_back(strlen(".sdk"));
  }
  return Name;
}

std::string ide::getSDKVersion(StringRef Path) {
  return getPlistEntry(llvm::Twine(Path)+"/System/Library/CoreServices/"
                       "SystemVersion.plist", "ProductBuildVersion");
}

// Modules failing to load are commented-out.
static const char *OSXModuleList[] = {
  "AGL",
  "AVFoundation",
  "AVKit",
  "Accelerate",
  "Accounts",
  "AddressBook",
  "AppKit",
  "AppKitScripting",
  "AppleScriptKit",
  "AppleScriptObjC",
  "ApplicationServices",
  "AudioToolbox",
  "AudioUnit",
  "AudioVideoBridging",
  "Automator",
  "CFNetwork",
  // "CalendarStore",
  "Carbon",
  "CloudKit",
  "Cocoa",
  "Collaboration",
  "Contacts",
  "CoreAudio",
  "CoreAudioKit",
  "CoreBluetooth",
  "CoreData",
  "CoreFoundation",
  "CoreGraphics",
  "CoreImage",
  "CoreLocation",
  "CoreMIDI",
  //  "CoreMIDIServer",
  "CoreMedia",
  "CoreMediaIO",
  "CoreServices",
  "CoreTelephony",
  "CoreText",
  "CoreVideo",
  "CoreWLAN",
  //  "CryptoTokenKit",
  //  "DVComponentGlue",
  "DVDPlayback",
  "Darwin",
  "DirectoryService",
  "DiscRecording",
  "DiscRecordingUI",
  "DiskArbitration",
  "Dispatch",
  //  "DrawSprocket",
  "EventKit",
  "ExceptionHandling",
  "FWAUserLib",
  "FinderSync",
  "ForceFeedback",
  "Foundation",
  "GLKit",
  "GLUT",
  "GSS",
  "GameController",
  "GameKit",
  "GameplayKit",
  "Hypervisor",
  //  "ICADevices",
  "IMServicePlugIn",
  "IOBluetooth",
  "IOBluetoothUI",
  "IOKit",
  "IOSurface",
  "ImageCaptureCore",
  "ImageIO",
  "InputMethodKit",
  // "InstallerPlugins",
  // "InstantMessage",
  // "JavaFrameEmbedding",
  "JavaScriptCore",
  // "JavaVM",
  // "Kerberos",
  // "LDAP",
  "LatentSemanticMapping",
  "LocalAuthentication",
  "MachO",
  "MapKit",
  "MediaAccessibility",
  "MediaLibrary",
  "MediaToolbox",
  //  "Message",
  "Metal",
  "MetalKit",
  "ModelIO",
  "MultipeerConnectivity",
  "NetFS",
  //  "NetworkExtension",
  "NotificationCenter",
  "OSAKit",
  "ObjectiveC",
  "OpenAL",
  "OpenCL",
  "OpenDirectory",
  "OpenGL",
  // "PCSC",
  "PreferencePanes",
  "PubSub",
  "Python",
  //  "QTKit", QTKit is unavailable on Swift.
  "Quartz",
  "QuartzCore",
  "QuickLook",
  "QuickTime",
  //  "Ruby",
  "SceneKit",
  "ScreenSaver",
  "Scripting",
  "ScriptingBridge",
  "Security",
  "SecurityFoundation",
  "SecurityInterface",
  // "ServiceManagement",
  "Social",
  "SpriteKit",
  "StoreKit",
  // "SyncServices",
  "SystemConfiguration",
  "TWAIN",
  "Tcl",
  // "VideoDecodeAcceleration",
  "VideoToolbox",
  "WebKit",
  "XPC",
  "libkern",
  "os",
  //  "vecLib",
  "vmnet",
};

// Modules failing to load are commented-out.
static const char *iOSModuleList[] = {
  "AVFoundation",
  "AVKit",
  "Accelerate",
  "Accounts",
  "AdSupport",
  "AddressBook",
  "AddressBookUI",
  "AssetsLibrary",
  "AudioToolbox",
  "AudioUnit",
  "CFNetwork",
  "CloudKit",
  "Contacts",
  "ContactsUI",
  "CoreAudio",
  "CoreAudioKit",
  "CoreBluetooth",
  "CoreData",
  "CoreFoundation",
  "CoreGraphics",
  "CoreImage",
  "CoreLocation",
  "CoreMIDI",
  "CoreMedia",
  "CoreMotion",
  "CoreSpotlight",
  "CoreTelephony",
  "CoreText",
  "CoreVideo",
  "Darwin",
  "Dispatch",
  "EventKit",
  "EventKitUI",
  "ExternalAccessory",
  "Foundation",
  "GLKit",
  "GSS",
  "GameController",
  "GameFoundation",
  "GameKit",
  "GameplayKit",
  "HealthKit",
  "HomeKit",
  "IMCommonCore",
  // "IOKit",
  "ImageIO",
  "JavaScriptCore",
  "LocalAuthentication",
  "MachO",
  "MapKit",
  "MediaAccessibility",
  "MediaPlayer",
  "MediaToolbox",
  "MessageUI",
  "MobileCoreServices",
  "ModelIO",
  "MultipeerConnectivity",
  "NetworkExtension",
  "NewsstandKit",
  "NotificationCenter",
  "ObjectiveC",
  "OpenAL",
  "OpenGLES",
  "PassKit",
  "Photos",
  "PhotosUI",
  "PushKit",
  "QuartzCore",
  "QuickLook",
  "SafariServices",
  "SceneKit",
  "ScreenRecorder",
  "Security",
  "Social",
  "SpriteKit",
  "StoreKit",
  "SystemConfiguration",
  "Twitter",
  "UIKit",
  "UIKit.UIGestureRecognizerSubclass",
  "VideoToolbox",
  "WatchConnectivity",
  "WatchKit",
  "WebKit",
  "iAd",
  "libkern",
  "os",
};

static const char *DeviceOnlyModuleList[] = {
  "Metal",
  "MetalKit",
  "MetalShaders",
};


static ArrayRef<const char *> getOSXModuleList() {
  return OSXModuleList;
}

static ArrayRef<const char *> getiOSModuleList() {
  return iOSModuleList;
}

static ArrayRef<const char *> getDeviceOnlyModuleList() {
  return DeviceOnlyModuleList;
}

void ide::collectModuleNames(StringRef SDKPath,
                               std::vector<std::string> &Modules) {
  std::string SDKName = getSDKName(SDKPath);
  std::string lowerSDKName = StringRef(SDKName).lower();
  bool isOSXSDK = StringRef(lowerSDKName).find("macosx") != StringRef::npos;
  bool isDeviceOnly = StringRef(lowerSDKName).find("iphoneos") != StringRef::npos;
  auto Mods = isOSXSDK ? getOSXModuleList() : getiOSModuleList();
  Modules.insert(Modules.end(), Mods.begin(), Mods.end());
  if (isDeviceOnly) {
    Mods = getDeviceOnlyModuleList();
    Modules.insert(Modules.end(), Mods.begin(), Mods.end());
  }
}

DeclNameViewer::DeclNameViewer(StringRef Text) {
  auto ArgStart = Text.find_first_of('(');
  if (ArgStart == StringRef::npos) {
    BaseName = Text;
    return;
  }
  BaseName = Text.substr(0, ArgStart);
  auto ArgEnd = Text.find_last_of(')');
  assert(ArgEnd != StringRef::npos);
  StringRef AllArgs = Text.substr(ArgStart + 1, ArgEnd - ArgStart - 1);
  AllArgs.split(Labels, ":");
  if (Labels.empty())
    return;
  assert(Labels.back().empty());
  Labels.pop_back();
}

unsigned DeclNameViewer::commonPartsCount(DeclNameViewer &Other) const {
  if (base() != Other.base())
    return 0;
  unsigned Result = 1;
  unsigned Len = std::min(args().size(), Other.args().size());
  for (unsigned I = 0; I < Len; ++ I) {
    if (args()[I] == Other.args()[I])
      Result ++;
    else
      return Result;
  }
  return Result;
}
