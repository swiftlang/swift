//===--- HeaderFooterLayout.h -----------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_HEADER_FOOTER_LAYOUT_H
#define SWIFT_BASIC_HEADER_FOOTER_LAYOUT_H

namespace swift {

template <class Header, class Footer, size_t TotalSize>
struct HeaderFooterLayoutPadding {
private:
  enum : ptrdiff_t {
    maxFooterOffset = TotalSize - (ptrdiff_t)sizeof(Footer),
    footerAlignment = (ptrdiff_t)alignof(Footer),
    footerOffset = maxFooterOffset - (maxFooterOffset % footerAlignment),
    size = footerOffset - (ptrdiff_t)sizeof(Header)
  };
  char padding[size];
};

template <class Header, class Footer, size_t TotalSize>
struct HeaderFooterLayout
    : Header,
      HeaderFooterLayoutPadding<Header, Footer, TotalSize>,
      Footer {};

} // namespace swift

#endif // SWIFT_BASIC_HEADER_FOOTER_LAYOUT_H

