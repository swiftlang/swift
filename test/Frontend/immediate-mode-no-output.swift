// RUN: rm -rf %t &&  mkdir -p %t && cd %t && echo >test.swift && %swift  -interpret test.swift &&  rm test.swift && test `ls | wc -w` -eq 0
