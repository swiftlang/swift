#ifndef BRIDGED_TYPEDEF_H
#define BRIDGED_TYPEDEF_H

@import Foundation;


typedef NSString *NSMyAmazingStringAlias __attribute__((swift_bridged_typedef));

void acceptNSMyAmazingStringAlias(NSMyAmazingStringAlias _Nullable param);
void acceptNSMyAmazingStringAliasArray(NSArray<NSMyAmazingStringAlias> * _Nonnull param);
void acceptIndirectedAmazingAlias(NSMyAmazingStringAlias _Nonnull * _Nullable param);

#endif
