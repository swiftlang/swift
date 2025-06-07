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
