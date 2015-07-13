#!/bin/bash
echo $LD_LIBRARY_PATH
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
echo $DIR
echo "export LD_LIBRARY_PATH=$DIR${LD_LIBRARY_PATH:+:}${LD_LIBRARY_PATH:-}" >> ~/.bashrc
source ~/.bashrc
echo $LD_LIBRARY_PATH
