// RUN: %empty-directory(%t/split)
// RUN: %empty-directory(%t/build)
// RUN: %{python} %utils/split_file.py -o %t/split %s

// RUN: %target-swift-frontend -emit-module -o %t/build %t/split/pck.swift

// First SPI completion then completion in file without SPI import
// RUN: %empty-directory(%t/cc-cache)
// RUN: %target-swift-ide-test -code-completion -completion-cache-path %t/cc-cache -source-filename %t/split/with-spi-import.swift -I %t/build -code-completion-token=COMPLETE | %FileCheck %s --check-prefix=WITH_SPI
// RUN: %target-swift-ide-test -code-completion -completion-cache-path %t/cc-cache -source-filename %t/split/with-different-spi-import.swift -I %t/build -code-completion-token=COMPLETE | %FileCheck %s --check-prefix=WITHOUT_SPI
// RUN: %target-swift-ide-test -code-completion -completion-cache-path %t/cc-cache -source-filename %t/split/with-normal-import.swift -I %t/build -code-completion-token=COMPLETE | %FileCheck %s --check-prefix=WITHOUT_SPI

// First completion in file without SPI import, then with SPI import
// RUN: %empty-directory(%t/cc-cache)
// RUN: %target-swift-ide-test -code-completion -completion-cache-path %t/cc-cache -source-filename %t/split/with-normal-import.swift -I %t/build -code-completion-token=COMPLETE | %FileCheck %s --check-prefix=WITHOUT_SPI
// RUN: %target-swift-ide-test -code-completion -completion-cache-path %t/cc-cache -source-filename %t/split/with-different-spi-import.swift -I %t/build -code-completion-token=COMPLETE | %FileCheck %s --check-prefix=WITHOUT_SPI
// RUN: %target-swift-ide-test -code-completion -completion-cache-path %t/cc-cache -source-filename %t/split/with-spi-import.swift -I %t/build -code-completion-token=COMPLETE | %FileCheck %s --check-prefix=WITH_SPI

// WITH_SPI-DAG: Decl[FreeFunction]/OtherModule[pck]: apiFunc()[#Void#]; name=apiFunc()
// WITH_SPI-DAG: Decl[FreeFunction]/OtherModule[pck]: spiFunc()[#Void#]; name=spiFunc()

// WITHOUT_SPI-NOT: spiFunc
// WITHOUT_SPI-DAG: Decl[FreeFunction]/OtherModule[pck]: apiFunc()[#Void#]; name=apiFunc()
// WITHOUT_SPI-NOT: spiFunc


// BEGIN pck.swift

public func apiFunc() {}

@_spi(MySPI)
public func spiFunc() {}

// BEGIN with-spi-import.swift

@_spi(MySPI) import pck

func test() {
  #^COMPLETE^#
}

// BEGIN with-different-spi-import.swift

@_spi(OtherSPI) import pck

func test() {
  #^COMPLETE^#
}


// BEGIN with-normal-import.swift

import pck

func test() {
  #^COMPLETE^#
}
