#!/bin/bash -e

if [ -z $TOOLCHAIN_PATH ]; then
    echo "Please provide the path to your toolchain via the TOOLCHAIN_PATH environment variable!"
    echo "  clang should be under \$TOOLCHAIN_PATH/bin"
    exit 1
fi
if [ -z $LIBRARY_PATH ]; then
    echo "Please provide the path to your libswiftCore via the LIBRARY_PATH environment variable!"
    echo "  libswiftCore should be under \$LIBRARY_PATH/swift/macosx"
    exit 1
fi
if [ -z $MACOSX_SDK ]; then
    echo "Please provide the path to your MacOS SDK via the MACOSX_SDK environment variable!"
    exit 1
fi

DEVICE_FLAG="-target x86_64-apple-macosx10.9 -sdk ${MACOSX_SDK} -toolchain-stdlib-rpath -Xlinker -rpath -Xlinker ${LIBRARY_PATH}"
RUN_LIB_PATH="${LIBRARY_PATH}/swift/macosx"

run_test() {
  ADD_FLAG=
  if [ "$1" == "emit-dead-strippable" ]
  then
    ADD_FLAG='-Xfrontend -emit-dead-strippable-symbols'
  elif [ "$1" == "emit-dead-strippable2" ]
  then
    ADD_FLAG='-Xfrontend -emit-dead-strippable-symbols -Xfrontend -disable-concrete-type-metadata-mangled-name-accessors'
  fi

  $TOOLCHAIN_PATH/bin/swiftc AnyDecodable.swift AnyCodable.swift AnyEncodable.swift main.swift -Osize -whole-module-optimization -o mytest -Xfrontend -disable-reflection-metadata $DEVICE_FLAG $ADD_FLAG -emit-ir -Xfrontend -disable-llvm-optzns -o mytest.ll &> tmp.log
  $TOOLCHAIN_PATH/bin/swiftc AnyDecodable.swift AnyCodable.swift AnyEncodable.swift main.swift -Osize -whole-module-optimization -o mytest -Xfrontend -disable-reflection-metadata $DEVICE_FLAG $ADD_FLAG -lto=llvm-full -g -Xlinker -object_path_lto -Xlinker . &> tmp.log
  codesign -f -s - mytest

  echo "  run"
  /usr/bin/env DYLD_LIBRARY_PATH=${LRUN_LIB_PAT} LD_LIBRARY_PATH=${RUN_LIB_PATH} SIMCTL_CHILD_DYLD_LIBRARY_PATH=${RUN_LIB_PATH} ./mytest &> tmp.log
  cat tmp.log
  error=$?
  if [ $error -ne 0 ]
  then
     echo "Error: unexpected run result"
     retcode=1
  fi

  #clean
  #rm -rf mytest *.a *.o *.ll *.bc *.log
}

retcode=0
echo "no emit-dead-strippable"
run_test
cp mytest mytest1
cp mytest.ll mytest1.ll
echo "emit-dead-strippable2"
run_test emit-dead-strippable2
cp mytest mytest2
cp mytest.ll mytest2.ll
echo "emit-dead-strippable"
run_test emit-dead-strippable
cp mytest mytest3
cp mytest.ll mytest3.ll
