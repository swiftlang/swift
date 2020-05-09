#ifndef __MACTYPES__
#define __MACTYPES__

#define STDLIB_TEST(TYPE, NAME) extern NAME NAME##_test
#define STDLIB_TYPEDEF(TYPE, NAME) \
  typedef TYPE NAME; \
  STDLIB_TEST(TYPE, NAME)

STDLIB_TYPEDEF(__INT8_TYPE__, SInt8);
STDLIB_TYPEDEF(__INT16_TYPE__, SInt16);
STDLIB_TYPEDEF(__INT32_TYPE__, SInt32);

STDLIB_TYPEDEF(__UINT8_TYPE__, UInt8);
STDLIB_TYPEDEF(__UINT16_TYPE__, UInt16);
STDLIB_TYPEDEF(__UINT32_TYPE__, UInt32);

#if !defined(_WIN32)
#include <stdint.h>
#endif

typedef SInt32                          Fixed;
typedef Fixed *                         FixedPtr;
typedef SInt32                          Fract;
typedef Fract *                         FractPtr;

typedef SInt16                          OSErr;
typedef SInt32                          OSStatus;
typedef unsigned long                   ByteCount;
typedef unsigned long                   ItemCount;
typedef UInt32                          FourCharCode;
typedef FourCharCode                    OSType;

typedef unsigned char                   Boolean;

enum {
  noErr                         = 0
};

typedef UInt32                          UnicodeScalarValue;
typedef UInt32                          UTF32Char;
typedef UInt16                          UniChar;
typedef UInt16                          UTF16Char;
typedef UInt8                           UTF8Char;
typedef UniChar *                       UniCharPtr;
typedef unsigned long                   UniCharCount;
typedef UniCharCount *                  UniCharCountPtr;

struct ProcessSerialNumber {
  UInt32              highLongOfPSN;
  UInt32              lowLongOfPSN;
};
typedef struct ProcessSerialNumber      ProcessSerialNumber;

typedef UInt8                           Byte;
typedef SInt8                           SignedByte;
Byte fakeAPIUsingByteInDarwin(void);

#endif
