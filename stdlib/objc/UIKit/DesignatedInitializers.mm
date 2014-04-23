//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains replacements for the designated initializers of some
// UIKit classes that take variadic parameters and thus cannot be used in
// Swift as-is.
//
//===----------------------------------------------------------------------===//

#import <UIKit/UIKit.h>

@interface UIActionSheet (_SwiftInterop)

- (instancetype)initWithTitle:(NSString *)title
                     delegate:(id<UIActionSheetDelegate>)delegate
            cancelButtonTitle:(NSString *)cancelButtonTitle
       destructiveButtonTitle:(NSString *)destructiveButtonTitle;

@end

@implementation UIActionSheet (_SwiftInterop)

- (instancetype)initWithTitle:(NSString *)title
                     delegate:(id<UIActionSheetDelegate>)delegate
            cancelButtonTitle:(NSString *)cancelButtonTitle
       destructiveButtonTitle:(NSString *)destructiveButtonTitle {
  return [self initWithTitle:title
                    delegate:delegate
           cancelButtonTitle:cancelButtonTitle
      destructiveButtonTitle:destructiveButtonTitle
           otherButtonTitles:nil];
}

@end


@interface UIAlertView (_SwiftInterop)

- (instancetype)initWithTitle:(NSString *)title
                      message:(NSString *)message
                     delegate:(id <UIAlertViewDelegate>)delegate
            cancelButtonTitle:(NSString *)cancelButtonTitle;

@end

@implementation UIAlertView (_SwiftInterop)

- (instancetype)initWithTitle:(NSString *)title
                      message:(NSString *)message
                     delegate:(id <UIAlertViewDelegate>)delegate
            cancelButtonTitle:(NSString *)cancelButtonTitle {
  return [self initWithTitle:title
                     message:message
                    delegate:delegate
           cancelButtonTitle:cancelButtonTitle
           otherButtonTitles:nil];
}

@end

