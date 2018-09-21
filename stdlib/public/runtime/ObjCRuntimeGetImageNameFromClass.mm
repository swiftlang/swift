//===--- ObjCRuntimeGetImageNameFromClass.cpp - ObjC hook setup -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Setup for the Objective-C runtime function class_getImageName, making it
// understand Swift classes. This is tricky because before Apple's 2018 OSs,
// this function was not designed to be hooked.
//
//===----------------------------------------------------------------------===//

#include "ObjCRuntimeGetImageNameFromClass.h"
#include "swift/Runtime/Config.h"

#if SWIFT_OBJC_INTEROP

#include "swift/Runtime/Metadata.h"

#include <dlfcn.h>
#include <objc/runtime.h>
#include <objc/message.h>
#include <TargetConditionals.h>

// Note: There are more #includes below under "Function patching machinery".
// Those are only relevant to the function patching machinery.

// On "embedded" targets (i.e. iOS/tvOS/watchOS devices), we need to
// patch +[NSBundle bundleForClass:] directly. The symbol table patch
// does not work for calls within the shared cache on those platforms,
// so the call within +bundleForClass: does not get patched. Instead,
// swizzle out the whole method with one that does the appropriate
// lookup for Swift classes. The symbol table patch handles this on Mac
// and simulators so this is not necessary there.
#if TARGET_OS_IPHONE && !TARGET_OS_SIMULATOR
#define PATCH_NSBUNDLE 1
#endif

using namespace swift;


// FIXME: This is from a later version of <objc/runtime.h>. Once the declaration
// is available in SDKs, we can remove this typedef.
typedef BOOL (*objc_hook_getImageName)(
    Class _Nonnull cls, const char * _Nullable * _Nonnull outImageName);

/// \see customGetImageNameFromClass
static objc_hook_getImageName defaultGetImageNameFromClass = nullptr;

/// Get the image name corresponding to a Swift class, accounting for
/// dynamically-initialized class metadata. Returns NO for ObjC classes.
static BOOL
getImageNameFromSwiftClass(Class _Nonnull objcClass,
                           const char * _Nullable * _Nonnull outImageName) {
  auto *classAsMetadata = reinterpret_cast<const ClassMetadata *>(objcClass);

  // Is this a Swift class?
  if (classAsMetadata->isTypeMetadata() &&
      !classAsMetadata->isArtificialSubclass()) {
    const void *descriptor = classAsMetadata->getDescription();
    assert(descriptor &&
           "all non-artificial Swift classes should have a descriptor");
    Dl_info imageInfo = {};
    if (!dladdr(descriptor, &imageInfo))
      return NO;
    *outImageName = imageInfo.dli_fname;
    return imageInfo.dli_fname != nullptr;
  }
  
  return NO;
}

/// A custom implementation of Objective-C's class_getImageName for Swift
/// classes, which knows how to handle dynamically-initialized class metadata.
///
/// Per the documentation for objc_setHook_getImageName, any non-Swift classes
/// will still go through the normal implementation of class_getImageName,
/// which is stored in defaultGetImageNameFromClass.
static BOOL
replacementGetImageNameFromClass(Class _Nonnull objcClass,
                                 const char * _Nullable * _Nonnull outImageName) {
  if (getImageNameFromSwiftClass(objcClass, outImageName))
    return YES;
  return defaultGetImageNameFromClass(objcClass, outImageName);
}

/***************************************************************************/
/* Function patching machinery *********************************************/
/***************************************************************************/

#include "llvm/ADT/ArrayRef.h"

#include <mach-o/dyld.h>
#include <mach-o/loader.h>
#include <mach-o/nlist.h>

#include <cstring>

using llvm::ArrayRef;

namespace {

#if __LP64__
# define LC_SEGMENT_COMMAND         LC_SEGMENT_64
# define LC_ROUTINES_COMMAND        LC_ROUTINES_64
  typedef struct mach_header_64     macho_header;
  typedef struct section_64         macho_section;
  typedef struct nlist_64           macho_nlist;
  typedef struct segment_command_64 macho_segment_command;
#else
# define LC_SEGMENT_COMMAND         LC_SEGMENT
# define LC_ROUTINES_COMMAND        LC_ROUTINES
  typedef struct mach_header        macho_header;
  typedef struct section            macho_section;
  typedef struct nlist              macho_nlist;
  typedef struct segment_command    macho_segment_command;
#endif

} // end anonymous namespace

/// Overwrite a cross-image symbol reference by directly editing symbol tables
/// in a Mach-O image.
///
/// This technique only works for certain versions of Apple's dynamic linker;
/// fortunately we only even attempt to invoke it when running on the OSs where
/// it works. Newer OSs already have the hook we need; older ones don't support
/// Swift at all.
///
/// Also, if the symbol being patched has references within the image where it
/// was originaly defined, those references will \e not be patched.
static void patchLazyPointers(const mach_header *mh, const char *symbolName,
                              const void *newValue) {
  // Get linkEditBase
  const uint32_t cmd_count = mh->ncmds;
  const load_command * const cmds =
    (const load_command *)((const char *)mh + sizeof(macho_header));
  const load_command *cmd;

  const uint8_t *linkEditBase = nullptr;
  intptr_t slide = 0;

  cmd = cmds;
  for (uint32_t i = 0; i < cmd_count; ++i) {
    if (cmd->cmd == LC_SEGMENT_COMMAND) {
      const macho_segment_command *seg = (const macho_segment_command *)cmd;
      if (strcmp(seg->segname, "__TEXT") == 0) 
        slide = (uintptr_t)mh - seg->vmaddr;
      else if (strcmp(seg->segname,"__LINKEDIT") == 0) 
        linkEditBase = (const uint8_t *)(seg->vmaddr + slide - seg->fileoff);
    }
    cmd = (const load_command *)(((const char *)cmd)+cmd->cmdsize);
  }
  if (linkEditBase == nullptr)
    return;

  // Gather symbol table info
  const macho_nlist *symbolTable = nullptr;
  const char *stringTable = nullptr;
  uint32_t stringTableBytes = 0;
  const uint32_t *indirectSymbolTable = nullptr;

  cmd = cmds;
  for (uint32_t i = 0; i < cmd_count; ++i) {
    switch (cmd->cmd) {
    case LC_SYMTAB: {
      const symtab_command *symtab = (const symtab_command *)cmd;
      stringTable = (const char *)&linkEditBase[symtab->stroff];
      stringTableBytes = symtab->strsize;
      symbolTable = (const macho_nlist *)(&linkEditBase[symtab->symoff]);
      break;
    }
    case LC_DYSYMTAB: {
      const dysymtab_command *dsymtab = (const dysymtab_command *)cmd;
      indirectSymbolTable =
          (const uint32_t *)(&linkEditBase[dsymtab->indirectsymoff]);
      break;
    }
    default:
      break;
    }
    cmd = (const load_command *)(((const char *)cmd)+cmd->cmdsize);
  }
  if (symbolTable == nullptr || stringTable == nullptr ||
      indirectSymbolTable == nullptr) {
    return;
  }

  // Find lazy pointer section
  cmd = cmds;
  for (uint32_t i = 0; i < cmd_count; ++i) {
    if (cmd->cmd == LC_SEGMENT_COMMAND) {
      const macho_segment_command *seg = (const macho_segment_command *)cmd;
      const macho_section * const sectionsStart = 
          (const macho_section *)(seg + 1);
      ArrayRef<macho_section> sections(sectionsStart, seg->nsects);

      for (const macho_section &sect : sections) {
        const uint8_t type = sect.flags & SECTION_TYPE;
        if (type != S_LAZY_SYMBOL_POINTERS)
          continue;
        
        const size_t pointerCount = sect.size / sizeof(uintptr_t);
        uintptr_t * const symbolPointers = (uintptr_t *)(sect.addr + slide);
        const uint32_t indirectTableOffset = sect.reserved1;
        for (uint32_t lazyIndex = 0; lazyIndex < pointerCount; ++lazyIndex) {
          uint32_t symbolIndex =
              indirectSymbolTable[indirectTableOffset + lazyIndex];
          if (symbolIndex >= stringTableBytes) {
            // Presumably INDIRECT_SYMBOL_LOCAL or some other special value.
            continue;
          }

          // Found symbol for this lazy pointer, now lookup address.
          const char *lazyTargetName = 
              &stringTable[symbolTable[symbolIndex].n_un.n_strx];
          if (strcmp(symbolName, lazyTargetName) == 0) {
            // Can't use the value currently stored here because it may 
            // be a dyld stub binder that will undo our patch if called.
            symbolPointers[lazyIndex] = (uintptr_t)newValue;
          }
        }
      }
    }
    cmd = (const load_command *)(((const char *)cmd)+cmd->cmdsize);
  }
}

/// \see callUnpatchedGetImageNameFromClass
static decltype(&class_getImageName) unpatchedGetImageNameFromClass = nullptr;

/// A fallback implementation of class_getImageName that just calls
/// unpatchedGetImageNameFromClass, with the signature of
/// objc_hook_getImageName.
///
/// This is used on older OSs where objc_setHook_getImageName isn't supported.
/// In this case, we invoke the Swift implementation above
/// (customGetImageNameFromClass), but set it up to fall back to this one.
/// Then this one can call the system's original version, which should be stored
/// in unpatchedGetImageNameFromClass.
static BOOL callUnpatchedGetImageNameFromClass(
    Class _Nonnull objcClass, const char * _Nullable * _Nonnull outImageName) {
  *outImageName = unpatchedGetImageNameFromClass(objcClass);
  return outImageName != nullptr;
}

/// A patched version of class_getImageName that always uses the Swift 
/// implementation.
///
/// The Swift implementation is always set up to chain to another
/// implementation, so on older OSs we just have to make sure that that chained
/// implementation is the original system version. See
/// callUnpatchedGetImageNameFromClass.
static const char *patchedGetImageNameFromClassForOldOSs(Class _Nullable cls) {
  if (!cls)
    return nullptr;
  const char *result;
  if (replacementGetImageNameFromClass(cls, &result))
    return result;
  return nullptr;
}

#if PATCH_NSBUNDLE
/// Selectors for the target method, patch method, and helper method.
#define BUNDLE_FOR_CLASS_SEL @selector(bundleForClass:)
#define PATCHED_BUNDLE_FOR_CLASS_SEL @selector(_swift_bundleForClass:)
#define BUNDLE_WITH_EXECUTABLE_PATH_SEL @selector(_swift_bundleWithExecutablePath:)

/// Whether the patch has already been done.
static bool didPatchNSBundle = false;

/// The patched version of +[NSBundle bundleForClass:]. If the class is
/// actually a Swift class and an image name can be retrieved from it,
/// look up the bundle based on that image name. Otherwise fall back to
/// the original version.
static id patchedBundleForClass(id self, SEL _cmd, Class objcClass) {
  const char *imageName;
  if (getImageNameFromSwiftClass(objcClass, &imageName)) {
    return ((id (*)(id, SEL, const char *))objc_msgSend)(
      self, BUNDLE_WITH_EXECUTABLE_PATH_SEL, imageName);
  }
  
  // Call through to the original, which is now found under the patched
  // selector.
  return ((id (*)(id, SEL, Class))objc_msgSend)(
    self, PATCHED_BUNDLE_FOR_CLASS_SEL, objcClass);
}

/// Install the patched +[NSBundle bundleForClass:].
static void patchNSBundle(void) {
  if (didPatchNSBundle) return;
  
  Class NSBundle = objc_getClass("NSBundle");
  if (!NSBundle) return;
  
  Method origMethod = class_getClassMethod(NSBundle, BUNDLE_FOR_CLASS_SEL);
  if (!origMethod) return;
  
  // Stuff can fail below, but if it does then we can't reasonably try again.
  didPatchNSBundle = true;
  
  BOOL success = class_addMethod(
    object_getClass(NSBundle), PATCHED_BUNDLE_FOR_CLASS_SEL,
    reinterpret_cast<IMP>(patchedBundleForClass), method_getTypeEncoding(origMethod));
  if (!success) return;
  
  Method patchMethod = class_getClassMethod(NSBundle, PATCHED_BUNDLE_FOR_CLASS_SEL);
  if (!patchMethod) return;
  
  method_exchangeImplementations(origMethod, patchMethod);
}
#endif

/// A hook for _dyld_register_func_for_add_image that overwrites any references
/// to class_getImageName with our custom implementation.
static void patchGetImageNameInImage(const struct mach_header *mh,
                                     intptr_t vmaddr_slide) {
  (void)vmaddr_slide;
  const void *newImplementationAddr =
      reinterpret_cast<const void *>(&patchedGetImageNameFromClassForOldOSs);
  patchLazyPointers(mh, "_class_getImageName", newImplementationAddr);
#if PATCH_NSBUNDLE
  patchNSBundle();
#endif
}

/***************************************************************************/
/* Installing the hook *****************************************************/
/***************************************************************************/

void swift::setUpObjCRuntimeGetImageNameFromClass() {
  assert(defaultGetImageNameFromClass == nullptr && "already set up");

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunguarded-availability"
  // If we're on a newer OS, install the customized class_getImageName through
  // the ObjC runtime.
  // Note: We're checking this the old-fashioned way instead of using @available
  // to make it easier to build from open-source.
  if (&objc_setHook_getImageName != nullptr) {
    objc_setHook_getImageName(replacementGetImageNameFromClass,
                              &defaultGetImageNameFromClass);
#pragma clang diagnostic pop

  } else {
    // On older OSs, manually patch in our new implementation of
    // class_getImageName, and set it up to chain to the original system 
    // version.

    // This assignment happens through a volatile pointer to make sure it occurs
    // before the later call to _dyld_register_func_for_add_image. (More
    // specifically, we need the original implementation of
    // 'class_getImageName', not the replaced one.)
    assert(unpatchedGetImageNameFromClass == nullptr);
    volatile auto *originalImplementationPtr = &unpatchedGetImageNameFromClass;
    *originalImplementationPtr = &class_getImageName;
    defaultGetImageNameFromClass = callUnpatchedGetImageNameFromClass;

    _dyld_register_func_for_add_image(&patchGetImageNameInImage);
  }
}

#endif // SWIFT_OBJC_INTEROP
