#import <Foundation/Foundation.h>

#ifdef SPI_AVAILABLE
#undef SPI_AVAILABLE
#define SPI_AVAILABLE API_AVAILABLE
#endif

#ifdef __SPI_AVAILABLE
#undef __SPI_AVAILABLE
#define __SPI_AVAILABLE API_AVAILABLE
#endif

SPI_AVAILABLE(macos(10.7))
@interface SPIInterface1
- (instancetype)init;
@end

__SPI_AVAILABLE(macos(10.7))
@interface SPIInterface2
- (instancetype)init;
@end

@interface SharedInterface
  + (NSInteger)foo SPI_AVAILABLE(macos(10.7));
@end
