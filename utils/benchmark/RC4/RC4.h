@interface RC4 : NSObject {
  uint8_t _i;
  uint8_t _j;
  uint8_t *_state;
}

- (id)initWithKey:(const unsigned char *)key length:(unsigned)len;

- (void)swapByIndex:(unsigned)x andIndex:(unsigned)y;

- (unsigned char) next;

@end



