@import Foundation;

@protocol UIApplicationDelegate <NSObject>
@end

@interface UIResponder : NSObject
@end

@interface UIView : UIResponder
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
