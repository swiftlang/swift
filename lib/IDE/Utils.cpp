//===--- Utils.cpp - Misc utilities ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/Utils.h"
#include "swift/Basic/SourceManager.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Parse/Parser.h"
#include "swift/Subsystems.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclObjC.h"
#include "llvm/ADT/STLExtras.h"
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
  const char *LineSourceStart = NULL;
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
      LineSourceStart = NULL;
      LineStart = p + 1;
      break;

    case '"':
      p = skipStringInCode (p, SourceEnd);
      break;

    case '{':
    case '(':
    case '[':
      ++LineIndent;
      if (LineSourceStart == NULL)
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
      if (LineSourceStart == NULL && !isspace(*p))
        LineSourceStart = p;
      break;
    }
    if (*p == '\0')
      break;
  }
  if (!IndentInfos.empty()) {
    SCR.IndentPrefix = std::move(IndentInfos.back().Prefix.str());
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
    for (auto Conf: const_cast<ValueDecl*>(CurrOver)->getConformances())
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
  return getPlistEntry(llvm::Twine(Path)+"/System/Library/CoreServices/        \
                       SystemVersion.plist", "ProductBuildVersion");
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
  //  "AppleShareClientCore",
  "ApplicationServices",
  "AudioToolbox",
  "AudioUnit",
  "AudioVideoBridging",
  "Automator",
  "CFNetwork",
  "CalendarStore",
  "Carbon",
  "CloudKit",
  "Cocoa",
  "Collaboration",
  "CoreAudio",
  "CoreAudioKit",
  //  "CoreAuthentication",
  "CoreBluetooth",
  "CoreData",
  "CoreFoundation",
  "CoreGraphics",
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
  //  "CoreWiFi",
  "CryptoTokenKit",
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
  "Hypervisor",
  "ICADevices",
  "IMServicePlugIn",
  "IOBluetooth",
  "IOBluetoothUI",
  "IOKit",
  "IOSurface",
  "ImageCaptureCore",
  "ImageIO",
  "InputMethodKit",
  "InstallerPlugins",
  "InstantMessage",
  "JavaFrameEmbedding",
  "JavaScriptCore",
  "JavaVM",
  "LDAP",
  "LatentSemanticMapping",
  "MachO",
  "MapKit",
  "MediaAccessibility",
  "MediaLibrary",
  "MediaToolbox",
  //  "Message",
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
  "PCSC",
  "PreferencePanes",
  "PubSub",
  "Python",
  //  "QTKit", QTKit is unavailable on Swift.
  "Quartz",
  "QuartzCore",
  "QuickLook",
  "QuickTime",
  //  "RubyCocoa",
  "SceneKit",
  "ScreenSaver",
  "Scripting",
  "ScriptingBridge",
  "Security",
  "SecurityFoundation",
  "SecurityInterface",
  "ServiceManagement",
  "Social",
  "SpriteKit",
  "StoreKit",
  "SyncServices",
  "SystemConfiguration",
  "TWAIN",
  "Tcl",
  "VideoDecodeAcceleration",
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
  //  "AcceleratorKit",
  "Accounts",
  "AdSupport",
  "AddressBook",
  "AddressBookUI",
  "AssetsLibrary",
  "AudioToolbox",
  "AudioUnit",
  "CFNetwork",
  "CloudKit",
  "CoreAudio",
  "CoreAudioKit",
  //  "CoreAuthentication",
  "CoreBluetooth",
  "CoreData",
  "CoreFoundation",
  "CoreGraphics",
  "CoreImage",
  "CoreLocation",
  "CoreMIDI",
  "CoreMedia",
  "CoreMotion",
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
  "GameKit",
  "HealthKit",
  "HomeKit",
  "ImageIO",
  "JavaScriptCore",
  "MachO",
  "MapKit",
  "MediaAccessibility",
  "MediaPlayer",
  "MediaToolbox",
  "MessageUI",
  "MobileCoreServices",
  "MultipeerConnectivity",
  "NetworkExtension",
  "NewsstandKit",
  "NotificationCenter",
  //  "NotificationsUI",
  "ObjectiveC",
  "OpenAL",
  "OpenGLES",
  "PassKit",
  "Photos",
  "PhotosUI",
  //  "PushRegistrar",
  "QuartzCore",
  "QuickLook",
  "SafariServices",
  "SceneKit",
  "Security",
  "Social",
  "SpriteKit",
  "StoreKit",
  "SystemConfiguration",
  "Twitter",
  "UIKit",
  "UIKit.UIGestureRecognizerSubclass",
  "VideoToolbox",
  "WatchKit",
  //  "XPC",
  "iAd",
  "libkern",
  "os",
};

static const char *DeviceOnlyModuleList[] = {
  "MetalImage",
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

