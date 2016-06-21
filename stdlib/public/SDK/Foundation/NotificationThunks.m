//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#import <Foundation/Foundation.h>

extern NS_RETURNS_RETAINED __nullable id
__NSNotificationCreate(
    NSString *NS_RELEASES_ARGUMENT __nonnull name,
    id NS_RELEASES_ARGUMENT __nullable object,
    NSDictionary *NS_RELEASES_ARGUMENT __nullable userInfo) {
  NSNotification *notif = [[NSNotification alloc] initWithName:name object:object userInfo:userInfo];
  [name release];
  [object release];
  [userInfo release];
  return notif;
}

extern NS_RETURNS_RETAINED __nullable id
__NSNotificationUserInfo(
  NSNotification *NS_RELEASES_ARGUMENT __nonnull notif) {
  NSDictionary *userInfo = [notif.userInfo retain]; // avoid the copy here since this is fetching the stored dictionary and copying it might destroy it's type
  [notif release];
  return userInfo;
}
