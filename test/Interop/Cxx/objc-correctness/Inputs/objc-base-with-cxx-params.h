#import <Foundation/Foundation.h>

class CxxRecord {
public:
    CxxRecord() : value(0) {}
    CxxRecord(int v) : value(v) {}
    CxxRecord(const CxxRecord &other) : value(other.value) {}
    ~CxxRecord() {}

    int value;
};

@interface ObjCBase : NSObject
- (CxxRecord)getRecord;
- (void)processRecord:(CxxRecord)record;
- (CxxRecord)transformRecord:(CxxRecord)input;
@end
