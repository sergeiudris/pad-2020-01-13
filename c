#!/bin/bash

link_spaces(){
    SPACE=pad
    mkdir -p spaces/$SPACE

    LIB=coll
    ln -s ../../src/$LIB/pad/$LIB spaces/$SPACE/$LIB
    
    LIB=core
    ln -s ../../src/$LIB/pad spaces/$SPACE/$LIB

    LIB=data
    ln -s ../../src/$LIB/pad/$LIB spaces/$SPACE/$LIB

    LIB=io
    ln -s ../../src/$LIB/pad/$LIB spaces/$SPACE/$LIB

    LIB=math
    ln -s ../../src/$LIB/pad/$LIB spaces/$SPACE/$LIB

    LIB=nrepl
    ln -s ../../src/$LIB/pad/$LIB spaces/$SPACE/$LIB

    LIB=prn
    ln -s ../../src/$LIB/pad/$LIB spaces/$SPACE/$LIB

   

}

"$@"