@import Foundation;

#include "stdint.h"

/*
void doit(NSArray<NSCopying> *array);
NSArray<NSCopying> *gimmeArray(void);
*/

void char_ptr_func(char *  _Nonnull buffer);
void const_char_ptr_func(const char *  _Nonnull buffer);

void unsigned_char_ptr_func(unsigned char * _Nonnull buffer);
void const_unsigned_char_ptr_func(const unsigned char * _Nonnull buffer);

void int_16_ptr_func(int16_t * _Nonnull buffer);
void int_32_ptr_func(int32_t * _Nonnull buffer);
void int_64_ptr_func(int64_t * _Nonnull buffer);

void const_int_16_ptr_func(const int16_t * _Nonnull buffer);
void const_int_32_ptr_func(const int32_t * _Nonnull buffer);
void const_int_64_ptr_func(const int64_t * _Nonnull buffer);

void uint_16_ptr_func(uint16_t * _Nonnull buffer);
void uint_32_ptr_func(uint32_t * _Nonnull buffer);
void uint_64_ptr_func(uint64_t * _Nonnull buffer);

void const_uint_16_ptr_func(const uint16_t * _Nonnull buffer);
void const_uint_32_ptr_func(const uint32_t * _Nonnull buffer);
void const_uint_64_ptr_func(const uint64_t * _Nonnull buffer);

/*
@interface LinkDescriptor
@end

@interface Test
- (BOOL)f:(NSArray<LinkDescriptor *> * _Nonnull)descriptors error:(NSError * __autoreleasing * __nullable)error;
- (void)f:(NSArray<LinkDescriptor *> * _Nonnull)descriptors completion:(nullable void (^)(BOOL success, NSError * _Nullable error))completion;
@end
*/
