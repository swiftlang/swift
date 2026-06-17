@import Foundation;

struct StructOfNSStrings {
  __unsafe_unretained NSString *nsstr;
};

struct StructOfBlocks {
  void (^__unsafe_unretained _Nonnull block)(void);
};

struct StrongsInAStruct { // expected-note {{record 'StrongsInAStruct' is not trivial to copy or destroy}}
  __strong NSString *nsstr;
};

struct WeaksInAStruct { // expected-note {{record 'WeaksInAStruct' is not trivial to copy or destroy}}
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
