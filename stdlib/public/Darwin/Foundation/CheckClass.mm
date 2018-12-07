#import <Foundation/Foundation.h>

#include <objc/runtime.h>

#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"

@interface NSKeyedUnarchiver (SwiftAdditions)
+ (int)_swift_checkClassAndWarnForKeyedArchiving:(Class)cls
                                       operation:(int)operation
     NS_SWIFT_NAME(_swift_checkClassAndWarnForKeyedArchiving(_:operation:));
@end

static bool isASCIIIdentifierChar(char c) {
  if (c >= 'a' && c <= 'z') return true;
  if (c >= 'A' && c <= 'Z') return true;
  if (c >= '0' && c <= '9') return true;
  if (c == '_') return true;
  if (c == '$') return true;
  return false;
}

template <typename T, size_t N>
static constexpr size_t arrayLength(T (&)[N]) { return N; }

static void logIfFirstOccurrence(Class objcClass, void (^log)(void)) {
  static auto queue = dispatch_queue_create(
      "SwiftFoundation._checkClassAndWarnForKeyedArchivingQueue",
      DISPATCH_QUEUE_SERIAL);
  static NSHashTable *seenClasses = nil;

  dispatch_sync(queue, ^{
    // Will be NO when seenClasses is still nil.
    if ([seenClasses containsObject:objcClass])
      return;

    if (!seenClasses) {
      NSPointerFunctionsOptions options = 0;
      options |= NSPointerFunctionsOpaqueMemory;
      options |= NSPointerFunctionsObjectPointerPersonality;
      seenClasses = [[NSHashTable alloc] initWithOptions:options capacity:16];
    }
    [seenClasses addObject:objcClass];

    // Synchronize logging so that multiple lines aren't interleaved.
    log();
  });
}

namespace {
  class StringRefLite {
    StringRefLite(const char *data, size_t len) : data(data), length(len) {}
  public:
    const char *data;
    size_t length;

    StringRefLite() : data(nullptr), length(0) {}

    template <size_t N>
    StringRefLite(const char (&staticStr)[N]) : data(staticStr), length(N) {}

    StringRefLite(swift::TypeNamePair rawValue)
        : data(rawValue.data),
          length(rawValue.length){}

    NS_RETURNS_RETAINED
    NSString *newNSStringNoCopy() const {
      return [[NSString alloc] initWithBytesNoCopy:const_cast<char *>(data)
                                            length:length
                                          encoding:NSUTF8StringEncoding
                                      freeWhenDone:NO];
    }

    const char &operator[](size_t offset) const {
      assert(offset < length);
      return data[offset];
    }

    StringRefLite slice(size_t from, size_t to) const {
      assert(from <= to);
      assert(to <= length);
      return {data + from, to - from};
    }

    const char *begin() const {
      return data;
    }
    const char *end() const {
      return data + length;
    }
  };
}

/// Assume that a non-generic demangled class name always ends in ".MyClass"
/// or ".(MyClass plus extra info)".
static StringRefLite findBaseName(StringRefLite demangledName) {
  size_t end = demangledName.length;
  size_t parenCount = 0;
  for (size_t i = end; i != 0; --i) {
    switch (demangledName[i - 1]) {
    case '.':
      if (parenCount == 0) {
        if (i != end && demangledName[i] == '(')
          ++i;
        return demangledName.slice(i, end);
      }
      break;
    case ')':
      parenCount += 1;
      break;
    case '(':
      if (parenCount > 0)
        parenCount -= 1;
      break;
    case ' ':
      end = i - 1;
      break;
    default:
      break;
    }
  }
  return {};
}

@implementation NSKeyedUnarchiver (SwiftAdditions)

/// Checks if class \p objcClass is good for archiving.
///
/// If not, a runtime warning is printed.
///
/// \param operation Specifies the archiving operation. Valid operations are:
/// 0:       archiving
/// 1:       unarchiving
/// \return Returns the status
/// 0:       not a problem class (either non-Swift or has an explicit name)
/// 1:       a Swift generic class
/// 2:       a Swift non-generic class where adding @objc is valid
/// Future versions of this API will return nonzero values for additional cases
/// that mean the class shouldn't be archived.
+ (int)_swift_checkClassAndWarnForKeyedArchiving:(Class)objcClass
                                       operation:(int)operation {
  using namespace swift;
  const ClassMetadata *theClass = (ClassMetadata *)objcClass;

  // Is it a (real) swift class?
  if (!theClass->isTypeMetadata() || theClass->isArtificialSubclass())
    return 0;

  // Does the class already have a custom name?
  if (theClass->getFlags() & ClassFlags::HasCustomObjCName)
    return 0;

  // Is it a mangled name?
  const char *className = class_getName(objcClass);
  if (!(className[0] == '_' && className[1] == 'T'))
    return 0;
  // Is it a name in the form <module>.<class>? Note: the module name could
  // start with "_T".
  if (strchr(className, '.'))
    return 0;

  // Is it a generic class?
  if (theClass->getDescription()->isGeneric()) {
    logIfFirstOccurrence(objcClass, ^{
      // Use actual NSStrings to force UTF-8.
      StringRefLite demangledName = swift_getTypeName(theClass,
                                                      /*qualified*/true);
      NSString *demangledString = demangledName.newNSStringNoCopy();
      NSString *mangledString = NSStringFromClass(objcClass);

      NSString *primaryMessage;
      switch (operation) {
      case 1:
        primaryMessage = [[NSString alloc] initWithFormat:
            @"Attempting to unarchive generic Swift class '%@' with mangled "
             "runtime name '%@'. Runtime names for generic classes are "
             "unstable and may change in the future, leading to "
             "non-decodable data.", demangledString, mangledString];
        break;
      default:
        primaryMessage = [[NSString alloc] initWithFormat:
            @"Attempting to archive generic Swift class '%@' with mangled "
             "runtime name '%@'. Runtime names for generic classes are "
             "unstable and may change in the future, leading to "
             "non-decodable data.", demangledString, mangledString];
        break;
      }
      NSString *generatedNote = [[NSString alloc] initWithFormat:
          @"To avoid this failure, create a concrete subclass and register "
           "it with NSKeyedUnarchiver.setClass(_:forClassName:) instead, "
           "using the name \"%@\".", mangledString];
      const char *staticNote =
          "If you need to produce archives compatible with older versions "
          "of your program, use NSKeyedArchiver.setClassName(_:for:) as well.";

      NSLog(@"%@", primaryMessage);
      NSLog(@"%@", generatedNote);
      NSLog(@"%s", staticNote);

      RuntimeErrorDetails::Note notes[] = {
        { [generatedNote UTF8String], /*numFixIts*/0, /*fixIts*/nullptr },
        { staticNote, /*numFixIts*/0, /*fixIts*/nullptr },
      };

      RuntimeErrorDetails errorInfo = {};
      errorInfo.version = RuntimeErrorDetails::currentVersion;
      errorInfo.errorType = "nskeyedarchiver-incompatible-class";
      errorInfo.notes = notes;
      errorInfo.numNotes = arrayLength(notes);

      _swift_reportToDebugger(RuntimeErrorFlagNone, [primaryMessage UTF8String],
                              &errorInfo);

      [primaryMessage release];
      [generatedNote release];
      [demangledString release];
    });
    return 1;
  }

  // It's a swift class with a (compiler generated) mangled name, which should
  // be written into an NSArchive.
  logIfFirstOccurrence(objcClass, ^{
    // Use actual NSStrings to force UTF-8.
    StringRefLite demangledName = swift_getTypeName(theClass,/*qualified*/true);
    NSString *demangledString = demangledName.newNSStringNoCopy();
    NSString *mangledString = NSStringFromClass(objcClass);

    NSString *primaryMessage;
    switch (operation) {
    case 1:
      primaryMessage = [[NSString alloc] initWithFormat:
          @"Attempting to unarchive Swift class '%@' with mangled runtime "
           "name '%@'. The runtime name for this class is unstable and may "
           "change in the future, leading to non-decodable data.",
          demangledString, mangledString];
      break;
    default:
      primaryMessage = [[NSString alloc] initWithFormat:
          @"Attempting to archive Swift class '%@' with mangled runtime "
           "name '%@'. The runtime name for this class is unstable and may "
           "change in the future, leading to non-decodable data.",
          demangledString, mangledString];
      break;
    }

    NSString *firstNote = [[NSString alloc] initWithFormat:
        @"You can use the 'objc' attribute to ensure that the name will not "
         "change: \"@objc(%@)\"", mangledString];

    StringRefLite baseName = findBaseName(demangledName);
    // Offer a more generic message if the base name we found doesn't look like
    // an ASCII identifier. This avoids printing names like "ABCモデル".
    if (baseName.length == 0 ||
        !std::all_of(baseName.begin(), baseName.end(), isASCIIIdentifierChar)) {
      baseName = "MyModel";
    }

    NSString *secondNote = [[NSString alloc] initWithFormat:
        @"If there are no existing archives containing this class, you should "
         "choose a unique, prefixed name instead: \"@objc(ABC%1$.*2$s)\"",
        baseName.data, (int)baseName.length];

    NSLog(@"%@", primaryMessage);
    NSLog(@"%@", firstNote);
    NSLog(@"%@", secondNote);

    // FIXME: We could suggest these as fix-its if we had source locations for
    // the class.
    RuntimeErrorDetails::Note notes[] = {
      { [firstNote UTF8String], /*numFixIts*/0, /*fixIts*/nullptr },
      { [secondNote UTF8String], /*numFixIts*/0, /*fixIts*/nullptr },
    };

    RuntimeErrorDetails errorInfo = {};
    errorInfo.version = RuntimeErrorDetails::currentVersion;
    errorInfo.errorType = "nskeyedarchiver-incompatible-class";
    errorInfo.notes = notes;
    errorInfo.numNotes = arrayLength(notes);

    _swift_reportToDebugger(RuntimeErrorFlagNone, [primaryMessage UTF8String],
                            &errorInfo);

    [primaryMessage release];
    [firstNote release];
    [secondNote release];
    [demangledString release];
  });
  return 2;
}
@end
