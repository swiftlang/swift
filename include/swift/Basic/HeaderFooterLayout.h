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

namespace detail {

template <ptrdiff_t size>
struct LayoutPadding {
  char padding[size];
};
template <>
struct LayoutPadding<0> {};

template <class Header, class Footer, size_t TotalSize>
struct HeaderFooterLayoutPaddingSize {
  enum : ptrdiff_t {
    maxFooterOffset = TotalSize - (ptrdiff_t)sizeof(Footer),
    footerAlignment = (ptrdiff_t)alignof(Footer),
    footerOffset = maxFooterOffset - (maxFooterOffset % footerAlignment),
    value = footerOffset - (ptrdiff_t)sizeof(Header)
  };
};

} // namespace detail

template <class Header, class Footer, size_t TotalSize>
struct HeaderFooterLayout
    : Header,
      detail::LayoutPadding<detail::HeaderFooterLayoutPaddingSize<
          Header, Footer, TotalSize>::value>,
      Footer {};

} // namespace swift

#endif // SWIFT_BASIC_HEADER_FOOTER_LAYOUT_H

