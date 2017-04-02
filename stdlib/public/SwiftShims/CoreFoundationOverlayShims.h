//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#import <CoreFoundation/CoreFoundation.h>

/// - Note: This file is intended to be used solely for the Foundation and CoreFoundation overlays in swift
/// any and all other uses are not supported. The Foundation team reserves the right to alter the contract,
/// calling convention, availability or any other facet of facilities offered in this file. If you use
/// anything from here and your app breaks, expect any bug to be marked as "behaves correctly".

#import "CFCharacterSetShims.h"
#import "CFHashingShims.h"
