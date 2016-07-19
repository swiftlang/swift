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

extern NS_RETURNS_RETAINED _Nullable id
__NSNotificationCreate(NSString *NS_RELEASES_ARGUMENT _Nonnull name,
                       id NS_RELEASES_ARGUMENT _Nullable object,
                       NSDictionary *NS_RELEASES_ARGUMENT _Nullable userInfo) {
  NSNotification *notif = [[NSNotification alloc] initWithName:name object:object userInfo:userInfo];
  [name release];
  [object release];
  [userInfo release];
  return notif;
}

extern NS_RETURNS_RETAINED _Nullable id
__NSNotificationUserInfo(NSNotification *NS_RELEASES_ARGUMENT _Nonnull notif) {
  NSDictionary *userInfo = [notif.userInfo retain]; // avoid the copy here since this is fetching the stored dictionary and copying it might destroy it's type
  [notif release];
  return userInfo;
}
