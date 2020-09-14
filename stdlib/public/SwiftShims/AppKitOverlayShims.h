//===--- AppKitOverlayShims.h ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===--------------------===//

#ifndef SWIFT_STDLIB_SHIMS_APPKIT_OVERLAY_H
#define SWIFT_STDLIB_SHIMS_APPKIT_OVERLAY_H

#include <TargetConditionals.h>

#if !TARGET_OS_IPHONE

@import AppKit;


//===--------------------===//
// diffable data source     //
//===--------------------===//

#if __has_feature(nullability)
#pragma clang assume_nonnull begin
#endif

typedef NSCollectionViewItem * _Nullable(^NSDiffableDataSourceCollectionViewItemProvider)(NSCollectionView*, NSIndexPath *indexPath, id identifier) API_UNAVAILABLE(ios);
typedef NSView<NSCollectionViewElement> * _Nullable(^NSDiffableDataSourceSupplementaryViewProvider)(NSCollectionView *, NSString *kind, NSIndexPath *indexPath) API_UNAVAILABLE(ios);

@class __NSDiffableDataSourceSnapshot;

API_AVAILABLE(macos(10.15)) API_UNAVAILABLE(ios)
@interface __NSDiffableDataSource : NSObject

- (instancetype)initWithNSCollectionView:(NSCollectionView*)nsCollectionView
                            itemProvider:(NSDiffableDataSourceCollectionViewItemProvider)itemProvider;

@property(nonatomic,weak,readonly,nullable) NSCollectionView *nsCollectionView;
@property(nonatomic,nullable,copy) NSDiffableDataSourceSupplementaryViewProvider nsSupplementaryViewProvider;

- (NSString*)description;

- (instancetype)init NS_UNAVAILABLE;

@property(nonatomic,readonly) NSInteger numberOfItems;
@property(nonatomic,readonly) NSInteger numberOfSections;
@property(nonatomic,readonly) NSArray *sectionIdentifiers;
@property(nonatomic,readonly) NSArray *itemIdentifiers;

- (NSInteger)numberOfItemsInSection:(id)sectionIdentifier;
- (NSArray*)itemIdentifiersInSectionWithIdentifier:(id)sectionIdentifier;
- (nullable id)sectionIdentifierForSectionContainingItemIdentifier:(id)identifier;

- (NSInteger)indexOfItemIdentifier:(id)itemIdentifier;
- (NSInteger)indexOfSectionIdentifier:(id)sectionIdentifier;

- (void)appendItemsWithIdentifiers:(NSArray*)identifiers;
- (void)appendItemsWithIdentifiers:(NSArray*)identifiers intoSectionWithIdentifier:(id _Nullable)sectionIdentifier;

- (void)insertItemsWithIdentifiers:(NSArray*)identifiers beforeItemWithIdentifier:(id)itemIdentifier;
- (void)insertItemsWithIdentifiers:(NSArray*)identifiers afterItemWithIdentifier:(id)itemIdentifier;

- (void)deleteItemsWithIdentifiers:(NSArray*)identifiers;
- (void)deleteAllItems;

- (void)moveItemWithIdentifier:(id)fromIdentifier beforeItemWithIdentifier:(id)toIdentifier;
- (void)moveItemWithIdentifier:(id)fromIdentifier afterItemWithIdentifier:(id)toIdentifier;

- (void)reloadItemsWithIdentifiers:(NSArray*)identifiers;

- (void)appendSectionsWithIdentifiers:(NSArray*)sectionIdentifiers;

- (void)insertSectionsWithIdentifiers:(NSArray*)sectionIdentifiers beforeSectionWithIdentifier:(id)toSectionIdentifier;
- (void)insertSectionsWithIdentifiers:(NSArray*)sectionIdentifiers afterSectionWithIdentifier:(id)toSectionIdentifier;

- (void)deleteSectionsWithIdentifiers:(NSArray*)sectionIdentifiers;

- (void)moveSectionWithIdentifier:(id)fromSectionIdentifier beforeSectionWithIdentifier:(id)toSectionIdentifier;
- (void)moveSectionWithIdentifier:(id)fromSectionIdentifier afterSectionWithIdentifier:(id)toSectionIdentifier;

- (void)reloadSectionsWithIdentifiers:(NSArray*)sectionIdentifiers;

- (nullable id)itemIdentifierForIndexPath:(NSIndexPath*)indexPath;
- (nullable NSIndexPath*)indexPathForItemIdentifier:(id)identifier;

- (__NSDiffableDataSourceSnapshot*)snapshot;
- (__NSDiffableDataSourceSnapshot*)emptySnapshot;
- (void)applyDifferencesFromSnapshot:(__NSDiffableDataSourceSnapshot*)snapshot;
- (void)reloadFromSnapshot:(__NSDiffableDataSourceSnapshot*)snapshot;
- (void)applyDifferencesFromSnapshot:(__NSDiffableDataSourceSnapshot *)snapshot animatingDifferences:(BOOL)animatingDifferences;
- (void)applyDifferencesFromSnapshot:(__NSDiffableDataSourceSnapshot *)snapshot animatingDifferences:(BOOL)animatingDifferences completion:(void(^ _Nullable)(void))completion;

@property(nonatomic,copy) NSDiffableDataSourceCollectionViewItemProvider collectionViewItemProvider;

- (NSInteger)_numberOfSectionsForNSCollectionView:(NSCollectionView*)collectionView NS_SWIFT_NAME(_numberOfSectionsForNSCollectionView(_:));
- (NSInteger)_numberOfItemsInSection:(NSInteger)section nsCollectionView:(NSCollectionView*)collectionView NS_SWIFT_NAME(_numberOfItemsInSection(_:nsCollectionView:));
- (NSCollectionViewItem *)_itemAtIndexPath:(NSIndexPath*)indexPath nsCollectionView:(NSCollectionView*)collectionView NS_SWIFT_NAME(_itemAtIndexPath(_:nsCollectionView:));
- (NSView *)_viewForSupplementaryElementOfKind:(NSString *)kind atIndexPath:(NSIndexPath *)indexPath nsCollectionView:(NSCollectionView *)collectionView NS_SWIFT_NAME(_viewForSupplementaryElementOfKind(_:atIndexPath:nsCollectionView:));

@end


API_AVAILABLE(macos(10.15)) API_UNAVAILABLE(ios)
@interface __NSDiffableDataSourceSnapshot : __NSDiffableDataSource<NSCopying>
- (instancetype)init;
@end

API_AVAILABLE(macos(10.15)) API_UNAVAILABLE(ios)
@interface NSDiffableDataSourceSnapshot()
@property(nonatomic,readonly) __NSDiffableDataSourceSnapshot *impl;
- (instancetype)initWithDataSource:(__NSDiffableDataSource * _Nullable)dataSource;
@end

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif

#endif // !TARGET_OS_IPHONE

#endif // SWIFT_STDLIB_SHIMS_APPKIT_OVERLAY_H
