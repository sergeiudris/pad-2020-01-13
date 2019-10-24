#!/bin/bash

link_spaces(){
    SPACE=pad
    mkdir -p spaces/$SPACE

    LIB=coll
    ln -s ../../src/$LIB/pad/$LIB spaces/$SPACE/$LIB

}

"$@"