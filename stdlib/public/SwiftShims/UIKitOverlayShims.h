//===--- UIKitOverlayShims.h ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===--------------------===//

#ifndef SWIFT_STDLIB_SHIMS_UIKIT_OVERLAY_H
#define SWIFT_STDLIB_SHIMS_UIKIT_OVERLAY_H

@import UIKit;

#if __has_feature(nullability)
#pragma clang assume_nonnull begin
#endif

#if TARGET_OS_TV || TARGET_OS_IOS
static inline BOOL _swift_UIKit_UIFocusEnvironmentContainsEnvironment(
                                                                      id<UIFocusEnvironment> environment,
                                                                      id<UIFocusEnvironment> otherEnvironment
                                                                      ) API_AVAILABLE(ios(11.0), tvos(11.0)) {
    return [UIFocusSystem environment:environment containsEnvironment:otherEnvironment];
}
#endif // TARGET_OS_TV || TARGET_OS_IOS

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif


//===--------------------===//
// diffable data source     //
//===--------------------===//

#if TARGET_OS_TV || TARGET_OS_IOS

#if __has_feature(nullability)
#pragma clang assume_nonnull begin
#endif

typedef UITableViewCell * _Nullable (^UITableViewDiffableDataSourceCellProvider)(UITableView * _Nonnull, NSIndexPath * _Nonnull, id _Nonnull);
typedef UICollectionViewCell * _Nullable (^UICollectionViewDiffableDataSourceCellProvider)(UICollectionView * _Nonnull,NSIndexPath * _Nonnull, id _Nonnull);

typedef UICollectionViewCell * _Nullable(^UIDiffableDataSourceCollectionViewCellProvider)(UICollectionView*, NSIndexPath *indexPath, id identifier);
typedef UICollectionReusableView * _Nullable(^UIDiffableDataSourceSupplementaryViewProvider)(UICollectionView *, NSString *kind, NSIndexPath *indexPath);

typedef NSString * _Nonnull(^UIDiffableDataSourceCellReuseIdentifierProvider)(id _Nonnull identifier);
typedef void(^UIDiffableDataSourceCollectionViewCellConfigurationHandler)(__kindof UICollectionViewCell  * _Nonnull , id _Nonnull identifier);

typedef NSString * _Nonnull(^UIDiffableDataSourceSupplementaryViewReuseIdentifierProvider)(NSString * _Nonnull kind, NSIndexPath * _Nonnull indexPath);
typedef void(^UIDiffableDataSourceSupplementaryViewConfigurationHandler)(__kindof UICollectionReusableView * _Nonnull, NSString * _Nonnull kind, NSIndexPath * _Nonnull indexPath);

typedef UITableViewCell * _Nullable(^UIDiffableDataSourceTableViewCellProvider)(__kindof UITableView * _Nonnull, NSIndexPath * _Nonnull, id _Nonnull identifier);
typedef void(^UIDiffableDataSourceTableViewCellConfigurationHandler)(__kindof UITableViewCell  * _Nonnull , id _Nonnull identifier);

@class __UIDiffableDataSourceSnapshot;

API_AVAILABLE(ios(13.0), tvos(13.0))
@interface __UIDiffableDataSource : NSObject

- (instancetype)initWithCollectionView:(UICollectionView*)collectionView
                          cellProvider:(UIDiffableDataSourceCollectionViewCellProvider)cellProvider;

- (instancetype)initWithCollectionView:(UICollectionView *)collectionView
                          cellProvider:(UIDiffableDataSourceCollectionViewCellProvider)cellProvider
                            dataSource:(id<UICollectionViewDataSource>)dataSource;

- (instancetype)initWithCollectionView:(UICollectionView*)collectionView
               reuseIdentifierProvider:(UIDiffableDataSourceCellReuseIdentifierProvider)cellReuseProvider
              cellConfigurationHandler:(UIDiffableDataSourceCollectionViewCellConfigurationHandler)cellConfigurationHandler;

- (NSString*)description;

- (instancetype)initWithTableView:(UITableView*)tableView
                     cellProvider:(UIDiffableDataSourceTableViewCellProvider)cellProvider;

- (instancetype)initWithTableView:(UITableView*)tableView
          reuseIdentifierProvider:(UIDiffableDataSourceCellReuseIdentifierProvider)cellResueProvider
         cellConfigurationHandler:(UIDiffableDataSourceTableViewCellConfigurationHandler)cellConfigurationHandler;

@property(nonatomic) UITableViewRowAnimation tableViewDefaultRowAnimation;
@property(nonatomic,weak,readonly,nullable) UITableView *tableView;
@property(nonatomic,copy) UITableViewDiffableDataSourceCellProvider tableViewCellProvider;

@property(nonatomic,weak,readonly,nullable) UICollectionView *collectionView;
@property(nonatomic,nullable,copy) UIDiffableDataSourceSupplementaryViewProvider supplementaryViewProvider;


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


- (__UIDiffableDataSourceSnapshot*)snapshot;
- (__UIDiffableDataSourceSnapshot*)emptySnapshot;
- (void)applyDifferencesFromSnapshot:(__UIDiffableDataSourceSnapshot*)snapshot;
- (void)reloadFromSnapshot:(__UIDiffableDataSourceSnapshot*)snapshot;
- (void)applyDifferencesFromSnapshot:(__UIDiffableDataSourceSnapshot *)snapshot animatingDifferences:(BOOL)animatingDifferences;


// deprecated

- (void)appendSectionWithIdentifier:(id)sectionIdentifier;
- (void)insertSectionWithIdentifier:(id)sectionIdentifier beforeSectionWithIdentifier:(id)toSectionIdentifier;
- (void)insertSectionWithIdentifier:(id)sectionIdentifier afterSectionWithIdentifier:(id)toSectionIdentifier;
- (void)applySnapshot:(__UIDiffableDataSourceSnapshot*)snapshot;


@property(nonatomic,nullable,copy) UIDiffableDataSourceSupplementaryViewReuseIdentifierProvider supplementaryReuseIdentifierProvider;
@property(nonatomic,nullable,copy) UIDiffableDataSourceSupplementaryViewConfigurationHandler supplementaryViewConfigurationHandler;

@property(nonatomic,copy) UICollectionViewDiffableDataSourceCellProvider collectionViewCellProvider;


// helpers

- (NSInteger)_numberOfSectionsForCollectionView:(UICollectionView*)collectionView NS_SWIFT_NAME(_numberOfSectionsForCollectionView(_:));
- (NSInteger)_numberOfItemsInSection:(NSInteger)section collectionView:(UICollectionView*)collectionView NS_SWIFT_NAME(_numberOfItemsInSection(_:collectionView:));
- (UICollectionViewCell*)_cellForItemAtIndexPath:(NSIndexPath*)indexPath collectionView:(UICollectionView*)collectionView NS_SWIFT_NAME(_cellForItemAtIndexPath(_:collectionView:));
- (UICollectionReusableView*)_viewForSupplementaryElementOfKind:(NSString *)kind atIndexPath:(NSIndexPath *)indexPath collectionView:(UICollectionView *)collectionView NS_SWIFT_NAME(_viewForSupplementaryElementOfKind(_:atIndexPath:collectionView:));

- (NSInteger)_numberOfSectionsForTableView:(UITableView*)tableView NS_SWIFT_NAME(_numberOfSectionsForTableView(_:));
- (NSInteger)_numberOfRowsInSection:(NSInteger)section tableView:(UITableView*)tableView NS_SWIFT_NAME(_numberOfRowsInSection(_:tableView:));
- (UITableViewCell*)_cellForRowAtIndexPath:(NSIndexPath*)indexPath tableView:(UITableView*)tableView NS_SWIFT_NAME(_cellForRowAtIndexPath(_:tableView:));

- (NSInteger)_numberOfSectionsForCollectionViewDeprecatedSPI:(UICollectionView*)collectionView NS_SWIFT_NAME(numberOfSections(for:));
- (NSInteger)_numberOfItemsInSectionDeprecatedSPI:(NSInteger)section collectionView:(UICollectionView*)collectionView NS_SWIFT_NAME(numberOfItems(inSection:collectionView:));
- (UICollectionViewCell*)_cellForItemAtIndexPathDeprecatedSPI:(NSIndexPath*)indexPath collectionView:(UICollectionView*)collectionView NS_SWIFT_NAME(cellForItem(at:collectionView:));
- (UICollectionReusableView*)_viewForSupplementaryElementOfKindDeprecatedSPI:(NSString *)kind atIndexPath:(NSIndexPath *)indexPath collectionView:(UICollectionView *)collectionView NS_SWIFT_NAME(viewForSupplementaryElement(ofKind:at:collectionView:));

- (NSInteger)_numberOfSectionsForTableViewDeprecatedSPI:(UITableView*)tableView NS_SWIFT_NAME(numberOfSections(for:));
- (NSInteger)_numberOfRowsInSectionDeprecatedSPI:(NSInteger)section tableView:(UITableView*)tableView NS_SWIFT_NAME(numberOfRows(inSection:tableView:));
- (UITableViewCell*)_cellForRowAtIndexPathDeprecatedSPI:(NSIndexPath*)indexPath tableView:(UITableView*)tableView NS_SWIFT_NAME(cellForRow(at:tableView:));

@end


API_AVAILABLE(ios(13.0), tvos(13.0))
@interface __UIDiffableDataSourceSnapshot : __UIDiffableDataSource<NSCopying>
- (instancetype)init;
@end

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif


#endif // TARGET_OS_TV || TARGET_OS_IOS

#endif // SWIFT_STDLIB_SHIMS_UIKIT_OVERLAY_H

