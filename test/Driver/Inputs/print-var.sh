#!/usr/bin/env bash
last_arg=${@: -1}
echo ${!last_arg}
