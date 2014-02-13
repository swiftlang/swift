
BUILD_DIR = $(HOME)/work/solon/llvm/build
SWIFT = $(BUILD_DIR)/bin/swift
CLANG = $(BUILD_DIR)/bin/clang

SDK := $(shell xcrun --show-sdk-path --sdk macosx)
OPTS = -O0 -g

%-swift.o: %.swift
	$(SWIFT) -sdk $(SDK) $(OUTPUT_OPTION) $< $(OPTS)

%.sil: %.swift
	$(SWIFT) -sdk $(SDK) $(OUTPUT_OPTION) $< $(OPTS) -emit-sil

%.ll: %.swift
	$(SWIFT) -sdk $(SDK) $(OUTPUT_OPTION) $< $(OPTS) -emit-llvm

%.s: %.swift
	$(SWIFT) -sdk $(SDK) $(OUTPUT_OPTION) $< $(OPTS) -S

%-swift: %-swift.o
	$(CLANG) $(OUTPUT_OPTION) -Wl,-rpath -Wl,"$(BUILD_DIR)/lib/swift/macosx" -L "$(BUILD_DIR)/lib/swift/macosx" $^

%-objc.o: %.m
	$(CLANG) $(OUTPUT_OPTION) $< $(OPTS) -isysroot $(SDK) -c

%-objc: %-objc.o
	$(CLANG) $(OUTPUT_OPTION) $< $(OPTS) -isysroot $(SDK) -framework Cocoa

Julia-swift: Julia-swift.o
Julia-objc: Julia-objc.o

.DEFAULT_GOAL = run
.PHONY = clean all run

clean:
	rm -rfv Julia-swift{,.o,.sil,.ll,.s} out.png Julia-objc{,.o,.sil,.ll,.s}

all: Julia-swift Julia-objc

run: Julia-swift Julia-objc
	@echo "Running julia swift."
	./Julia-swift
	@echo "Running julia objc."
	./Julia-objc
