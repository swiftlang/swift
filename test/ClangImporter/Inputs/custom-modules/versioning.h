#pragma once
extern const int kVersion;
extern const int kVersion1;
extern const int kVersion2;
#if OLD_DEPLOYMENT_TARGET
#define kVersion kVersion1
#else
#define kVersion kVersion2
#endif
