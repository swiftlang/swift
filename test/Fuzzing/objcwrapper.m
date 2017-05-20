// objcwrapper.m

#import <Foundation/Foundation.h>

@interface Fuzzer : NSObject
+ (void)fuzzOneInputWithData:(char const * _Nonnull)Data Size:(long)Size;
@end

int LLVMFuzzerTestOneInput(char* data, long size) {
    [Fuzzer fuzzOneInputWithData:data Size:size];
    return 0;
}
