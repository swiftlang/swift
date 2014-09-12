#!/bin/sh
# parseable_output_unicode_runner.sh

"$SWIFTC" -emit-executable "$INPUT_DIR/你好.swift" -o "$OUTPUT_BASE.out" -emit-module -emit-module-path "$OUTPUT_BASE.swiftmodule" -emit-objc-header-path "$OUTPUT_BASE.h" -serialize-diagnostics -emit-dependencies -parseable-output -driver-skip-execution
