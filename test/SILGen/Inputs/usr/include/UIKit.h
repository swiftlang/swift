@import Foundation;

@protocol UIApplicationDelegate
@end

#ifdef SILGEN_TEST_UIAPPLICATIONMAIN_NULLABILITY
int UIApplicationMain(int argc, char *_Nullable *_Nonnull argv,
                      NSString *_Nullable principalClassName, 
                      NSString *_Nullable delegateClassName);
#else
int UIApplicationMain(int argc, char **argv,
                      NSString *principalClassName, 
                      NSString *delegateClassName);
#endif
