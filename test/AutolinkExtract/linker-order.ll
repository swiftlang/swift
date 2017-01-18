; RUN: llc -mtriple armv7--linux-gnueabihf -filetype obj -o - %s | %target-swift-autolink-extract -o - - | %FileCheck %s
; RUN: llc -mtriple x86_64--windows-gnu -filetype obj -o - %s | %target-swift-autolink-extract -o - - | %FileCheck %s
; RUN: llc -mtriple x86_64--windows-cygnus -filetype obj -o - %s | %target-swift-autolink-extract -o - - | %FileCheck %s
; REQUIRES: autolink-extract

; Ensure that the options in the object file preserve ordering.  The linker
; options are order dependent, and we would accidentally reorder them because we
; used a std::set rather than an llvm::SmallSetVector.

@_swift1_autolink_entries_1 = private constant [7 x i8] c"Saleem\00", section ".swift1_autolink_entries", align 8
@_swift1_autolink_entries_0 = private constant [8 x i8] c"Naureen\00", section ".swift1_autolink_entries", align 8

@_swift1_autolink_entries_2 = private constant [7 x i8] c"-rpath\00", section ".swift1_autolink_entries", align 8
@_swift1_autolink_entries_3 = private constant [8 x i8] c"Naureen\00", section ".swift1_autolink_entries", align 8
@_swift1_autolink_entries_4 = private constant [7 x i8] c"-rpath\00", section ".swift1_autolink_entries", align 8
@_swift1_autolink_entries_5 = private constant [7 x i8] c"Saleem\00", section ".swift1_autolink_entries", align 8

; CHECK: Saleem
; CHECK: Naureen

; CHECK: -rpath
; CHECK: Naureen

; CHECK: -rpath
; CHECK: Saleem

