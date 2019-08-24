@import Foundation;

@protocol UIApplicationDelegate <NSObject>
@end

@interface UIResponder : NSObject
@end

@interface UIView : UIResponder
@end

typedef NS_OPTIONS(NSUInteger, UIViewAnimationOptions) {
    UIViewAnimationOptionLayoutSubviews            = 1 <<  0,
    UIViewAnimationOptionTransitionFlipFromBottom  = 7 << 20,
  };
  
@interface UIView(UIViewAnimationWithBlocks)

+ (void)animateWithDuration:(NSTimeInterval)duration 
                      delay:(NSTimeInterval)delay 
                    options:(UIViewAnimationOptions)options 
                 animations:(void (^)(void))animations 
                 completion:(void (^)(BOOL finished))completion;

@end

@interface UIActionSheet : UIView

- (instancetype)initWithTitle:(NSString *)title
                     delegate:(id)delegate
            cancelButtonTitle:(NSString *)cancelButtonTitle
       destructiveButtonTitle:(NSString *)destructiveButtonTitle
            otherButtonTitles:(NSString *)titles, ...;

@end

@interface UIAlertView : UIView

- (instancetype)initWithTitle:(NSString *)title
                      message:(NSString *)message
                     delegate:(id)delegate
            cancelButtonTitle:(NSString *)cancelButtonTitle
            otherButtonTitles:(NSString *)titles, ...;

@end
