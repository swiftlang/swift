@import Foundation;

struct StructOfNSStrings {
  __unsafe_unretained NSString *nsstr;
};

struct StructOfBlocks {
  void (^__unsafe_unretained _Nonnull block)(void);
};

struct StrongsInAStruct { // expected-note {{record 'StrongsInAStruct' contains strong references and cannot be imported without enabling experimental feature ImportCStructsWithArcFields}}
  __strong NSString *nsstr;
};

struct WeaksInAStruct { // expected-note {{record 'WeaksInAStruct' contains a weak reference and cannot be imported}}
  __weak NSString *nsstr;
};

// Types used when ImportCStructsWithArcFields is enabled.
@interface MYObject : NSObject
@end

struct StrongsInAStructArc {
  __strong MYObject *_Nonnull myobj;
};

void takeStrongArcStruct(struct StrongsInAStructArc s);
struct StrongsInAStructArc returnStrongArcStruct(void);

struct UnavailableArcStruct {
  __strong MYObject *_Nonnull myobj;
} NS_SWIFT_UNAVAILABLE("Use MySwiftType instead");

struct CNameForRenamedArcStruct {
  __strong MYObject *_Nonnull myobj;
} __attribute__((swift_name("RenamedArcStruct")));

// Nested struct with strong ARC fields should be imported.
struct InnerArcStruct {
  __strong MYObject *_Nonnull inner;
};
struct OuterArcStruct {
  struct InnerArcStruct nested;
  int tag;
};

// Struct with __weak fields should not be imported.
struct WeakInAStructArc {
  __weak MYObject *_Nullable weakobj;
};

// Struct nesting a __weak field should not be imported.
struct OuterWithWeakInner {
  struct WeakInAStructArc nested;
};

// Union with strong ARC fields should not be imported.
union UnionWithStrong {
  __strong MYObject *_Nonnull obj;
  void *_Nonnull raw;
};

// Struct containing a union with strong ARC fields should not be imported.
struct StructWithArcUnion {
  union UnionWithStrong u;
};
