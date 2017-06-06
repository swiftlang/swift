#import <Foundation/Foundation.h>

#include "swift/Runtime/Metadata.h"

@interface NSKeyedUnarchiver (SwiftAdditions)
+ (int)_swift_checkClassAndWarnForKeyedArchiving:(Class)cls
                                       operation:(int)operation
     NS_SWIFT_NAME(_swift_checkClassAndWarnForKeyedArchiving(_:operation:));
@end

@implementation NSKeyedUnarchiver (SwiftAdditions)

/// Checks if class \p cls is good for archiving.
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
+ (int)_swift_checkClassAndWarnForKeyedArchiving:(Class)cls
                                       operation:(int)operation {
  const swift::ClassMetadata *theClass = (swift::ClassMetadata *)cls;

  // Is it a (real) swift class?
  if (!theClass->isTypeMetadata() || theClass->isArtificialSubclass())
    return 0;

  // Does the class already have a custom name?
  if (theClass->getFlags() & swift::ClassFlags::HasCustomObjCName)
    return 0;

  const char *className = [NSStringFromClass(cls) UTF8String];

  // Is it a mangled name?
  if (!(className[0] == '_' && className[1] == 'T'))
    return 0;
  // Is it a name in the form <module>.<class>? Note: the module name could
  // start with "_T".
  if (strchr(className, '.'))
    return 0;

  // Is it a generic class?
  if (theClass->getDescription()->GenericParams.isGeneric()) {
    // TODO: print a warning
    return 1;
  }

  // It's a swift class with a (compiler generated) mangled name, which should
  // be written into an NSArchive.
  // TODO: print a warning
  return 2;
}
@end
