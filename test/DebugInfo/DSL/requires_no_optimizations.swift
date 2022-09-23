// RUN: %target-swift-frontend %s -Xllvm -sil-print-debuginfo -emit-sil -g -Onone -disable-availability-checking | %FileCheck %s

